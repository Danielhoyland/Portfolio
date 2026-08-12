// ==UserScript==
// @name         Blackboard Full Downloader — Clean Structure v5.0
// @namespace    http://tampermonkey.net/
// @version      5.0
// @description  Downloads Blackboard course with correct folder structure and auto-cleanup
// @match        https://ntnu.blackboard.com/*
// @grant        GM_xmlhttpRequest
// @grant        GM_download
// @require      https://unpkg.com/fflate@0.8.0
// ==/UserScript==

(function() {
'use strict';

/* -------------------------------------------------------
   CONFIGURATION
------------------------------------------------------- */
const CONFIG = {
    MAX_PARALLEL_DOWNLOADS: 3,        // Lower parallelism for better reliability
    REQUEST_DELAY: 300,               // Polite delay between requests (ms)
    RETRY_ATTEMPTS: 3,
    RETRY_DELAY: 1000,
    SKIP_DUPLICATE_CONTENT: true,     // Skip files with identical content
    CLEAR_PREVIOUS_DATA: true,        // Delete old course folder on start
};

/* -------------------------------------------------------
   GLOBAL STATE
------------------------------------------------------- */
let currentCourseName = "Blackboard Course";
let currentCourseId = null;
let totalFiles = 0;
let processedFiles = 0;
let failedFiles = [];
let isRunning = false;

// Each file object carries its own path for correct placement
let allFiles = []; // { url, filename, path: [] }

// Deduplication sets
const visitedPages = new Set();
const seenFingerprints = new Set();
const seenUrls = new Set();
const filenameCountMap = new Map();

/* -------------------------------------------------------
   UI (unchanged, just updated version number)
------------------------------------------------------- */
function addEnhancedUI() {
    const panel = document.createElement('div');
    panel.id = 'bb-downloader-panel';
    panel.innerHTML = `
        <style>
            #bb-downloader-panel {
                position: fixed; top: 10px; right: 10px; z-index: 999999;
                background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                border-radius: 12px; padding: 15px;
                box-shadow: 0 10px 40px rgba(0,0,0,0.3); color: white;
                font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
                min-width: 300px; max-width: 400px;
            }
            #bb-downloader-panel.minimized .content { display: none; }
            #bb-downloader-panel h3 { margin: 0 0 10px 0; font-size: 16px; cursor: pointer;
                display: flex; justify-content: space-between; align-items: center; }
            #bb-downloader-panel button {
                background: white; color: #667eea; border: none; padding: 8px 16px;
                border-radius: 6px; cursor: pointer; font-weight: 600; margin: 5px; width: 100%;
                transition: transform 0.1s ease;
            }
            #bb-downloader-panel button:hover { transform: scale(1.02); }
            #bb-downloader-panel button:disabled { opacity: 0.6; cursor: not-allowed; transform: none; }
            #bb-downloader-panel .progress-bar {
                width: 100%; height: 6px; background: rgba(255,255,255,0.2);
                border-radius: 3px; margin: 10px 0; overflow: hidden;
            }
            #bb-downloader-panel .progress-fill {
                height: 100%; background: #4ade80; border-radius: 3px;
                transition: width 0.3s ease; width: 0%;
            }
            #bb-downloader-panel .stats { font-size: 12px; opacity: 0.9; margin: 5px 0; }
        </style>
        <h3>
            📥 BB Downloader v5
            <button style="background:transparent;border:1px solid rgba(255,255,255,0.3);color:white;padding:4px 8px;font-size:12px;width:auto;margin:0;"
                    onclick="document.getElementById('bb-downloader-panel').classList.toggle('minimized')">_</button>
        </h3>
        <div class="content">
            <div class="stats" id="bb-stats">Ready to download</div>
            <div class="progress-bar"><div class="progress-fill" id="bb-progress"></div></div>
            <button id="bb-download-btn">🚀 Start Download</button>
            <button id="bb-zip-btn" disabled>📦 Export as ZIP</button>
            <button id="bb-cancel-btn" style="display:none;">⏹️ Cancel</button>
        </div>
    `;
    document.body.appendChild(panel);
    document.getElementById('bb-download-btn').addEventListener('click', startDownload);
    document.getElementById('bb-zip-btn').addEventListener('click', exportZip);
    document.getElementById('bb-cancel-btn').addEventListener('click', cancelDownload);
}

function updateUI(stats = {}) {
    const statsEl = document.getElementById('bb-stats');
    const progressEl = document.getElementById('bb-progress');
    const downloadBtn = document.getElementById('bb-download-btn');
    const zipBtn = document.getElementById('bb-zip-btn');
    const cancelBtn = document.getElementById('bb-cancel-btn');

    if (stats.status === 'running') {
        downloadBtn.style.display = 'none';
        cancelBtn.style.display = 'block';
        const percent = stats.total > 0 ? ((stats.processed / stats.total) * 100).toFixed(1) : 0;
        statsEl.textContent = `Downloading: ${stats.processed}/${stats.total} files (${stats.failed} failed)`;
        progressEl.style.width = `${percent}%`;
    } else if (stats.status === 'complete') {
        downloadBtn.style.display = 'block';
        downloadBtn.textContent = '🔄 Download Again';
        cancelBtn.style.display = 'none';
        zipBtn.disabled = false;
        statsEl.textContent = `Complete! ${stats.processed} files (${stats.failed} failed)`;
        progressEl.style.width = '100%';
    } else {
        downloadBtn.style.display = 'block';
        cancelBtn.style.display = 'none';
        zipBtn.disabled = true;
    }
}

/* -------------------------------------------------------
   HELPER: PAGE KEY
------------------------------------------------------- */
function getPageKey(url) {
    try {
        const u = new URL(url);
        const params = new URLSearchParams(u.search);
        if (params.has('content_id')) return 'content:' + params.get('content_id');
        if (params.has('toc_id')) return 'toc:' + params.get('toc_id');
        return u.pathname + '?' + params.toString();
    } catch { return url; }
}

/* -------------------------------------------------------
   PARSING FUNCTIONS
------------------------------------------------------- */
function getCourseSections(doc) {
    const sections = [];
    const seen = new Set();
    for (const a of doc.querySelectorAll('#courseMenuPalette_contents li a')) {
        const href = a.getAttribute('href');
        if (!href || !href.includes('course_id=')) continue;
        const url = new URL(href, location.origin).href;
        const key = getPageKey(url);
        if (seen.has(key)) continue;
        seen.add(key);
        const titleSpan = a.querySelector('span[title]');
        const name = titleSpan ? titleSpan.getAttribute('title') : a.textContent.trim();
        if (name && name.length > 2) sections.push({ name, url, pageKey: key });
    }
    return sections;
}

function getFolders(doc) {
    const folders = [];
    const seen = new Set();
    const contentArea = doc.querySelector('#content_listContainer') || doc;
    for (const li of contentArea.querySelectorAll("li")) {
        const img = li.querySelector("img");
        if (!img) continue;
        const isFolder = img.alt === "Content Folder" || img.src?.includes('folder') || img.className?.includes('folder');
        if (!isFolder) continue;
        const a = li.querySelector("a[href*='listContent.jsp']");
        if (!a) continue;
        const url = new URL(a.href, location.origin).href;
        const key = getPageKey(url);
        if (seen.has(key)) continue;
        seen.add(key);
        folders.push({ name: a.textContent.trim(), url, pageKey: key });
    }
    return folders;
}

function getFiles(doc, currentPath) {
    const files = [];
    const seenFileUrls = new Set();
    const selectors = [
        'a[href*="/bbcswebdav/"]', 'a[href*="download"]', 'a[href*="pid-"]',
        'a[href$=".pdf"]', 'a[href$=".doc"]', 'a[href$=".docx"]', 'a[href$=".ppt"]',
        'a[href$=".pptx"]', 'a[href$=".xls"]', 'a[href$=".xlsx"]', 'a[href$=".zip"]',
        'a[href$=".png"]', 'a[href$=".jpg"]', 'a[href$=".jpeg"]', 'a[href$=".gif"]',
        'a[href$=".mp4"]'
    ];
    for (const sel of selectors) {
        for (const a of doc.querySelectorAll(sel)) {
            const href = a.getAttribute('href');
            if (!href) continue;
            const url = new URL(href, location.origin).href;
            if (url.includes('listContent.jsp') || url.includes('courseMain') || url === location.href) continue;
            if (seenFileUrls.has(url)) continue;
            seenFileUrls.add(url);
            const filename = extractFilenameFromUrl(url) || a.textContent.trim() || 'unnamed_file';
            files.push({ url, filename, path: [...currentPath] });
        }
    }
    return files;
}

function extractFilenameFromUrl(url) {
    try {
        const u = new URL(url);
        const nameParam = u.searchParams.get('fileName') || u.searchParams.get('filename');
        if (nameParam) return decodeURIComponent(nameParam);
        const pathname = u.pathname;
        const filename = pathname.split('/').pop();
        if (filename && filename.includes('.') && !filename.endsWith('.jsp') && !filename.endsWith('.do'))
            return decodeURIComponent(filename);
        const parts = pathname.split('/');
        for (let i = parts.length-1; i >=0; i--)
            if (parts[i].includes('.') && parts[i].length > 4) return decodeURIComponent(parts[i]);
        return null;
    } catch { return null; }
}

/* -------------------------------------------------------
   FINGERPRINT & DOWNLOAD
------------------------------------------------------- */
async function fingerprint(bytes) {
    const len = bytes.length;
    const first = bytes.slice(0, Math.min(1024, len));
    const last = bytes.slice(Math.max(0, len - 1024), len);
    const combined = new Uint8Array(first.length + last.length + 8);
    combined.set(first, 0);
    combined.set(last, first.length);
    const lenBytes = new Uint8Array(new BigInt64Array([BigInt(len)]).buffer);
    combined.set(lenBytes, first.length + last.length);
    const hashBuffer = await crypto.subtle.digest("SHA-256", combined);
    return Array.from(new Uint8Array(hashBuffer)).map(b => b.toString(16).padStart(2, '0')).join('');
}

async function downloadWithRetry(url, retries = CONFIG.RETRY_ATTEMPTS) {
    for (let i = 0; i < retries; i++) {
        try { return await downloadFileData(url); }
        catch (e) {
            console.warn(`Retry ${i+1} for ${url}`, e);
            if (i < retries-1) await sleep(CONFIG.RETRY_DELAY * (i+1));
            else throw e;
        }
    }
}

function downloadFileData(url) {
    return new Promise((resolve, reject) => {
        GM_xmlhttpRequest({
            method: "GET", url, responseType: "arraybuffer", timeout: 60000,
            onload: function(res) {
                if (res.status >= 200 && res.status < 300) {
                    let fname = null;
                    const disp = res.responseHeaders?.match(/filename\*?=(?:UTF-8'')?([^;\n]+)/i);
                    if (disp) fname = decodeURIComponent(disp[1].trim());
                    resolve({ bytes: new Uint8Array(res.response), filename: fname });
                } else reject(new Error(`HTTP ${res.status}`));
            },
            onerror: (e) => reject(new Error('Network error')),
            ontimeout: () => reject(new Error('Timeout'))
        });
    });
}

function fixFileExtension(filename, bytes) {
    const lower = filename.toLowerCase();
    if (bytes[0]==0x25 && bytes[1]==0x50 && bytes[2]==0x44 && bytes[3]==0x46 && !lower.endsWith('.pdf')) return filename+'.pdf';
    if (bytes[0]==0x50 && bytes[1]==0x4B && !lower.endsWith('.zip')) return filename+'.zip';
    if (bytes[0]==0x89 && bytes[1]==0x50 && bytes[2]==0x4E && bytes[3]==0x47 && !lower.endsWith('.png')) return filename+'.png';
    return filename;
}

function handleDuplicateFilename(filename) {
    const lower = filename.toLowerCase();
    if (filenameCountMap.has(lower)) {
        const count = filenameCountMap.get(lower) + 1;
        filenameCountMap.set(lower, count);
        const dot = filename.lastIndexOf('.');
        return dot > 0 ? filename.slice(0,dot)+`_${count}`+filename.slice(dot) : filename+`_${count}`;
    } else {
        filenameCountMap.set(lower, 1);
        return filename;
    }
}

/* -------------------------------------------------------
   FILE SAVE (uses file.path)
------------------------------------------------------- */
async function saveFile(filePath, filename, bytes) {
    const root = await navigator.storage.getDirectory();
    let current = await root.getDirectoryHandle(sanitize(currentCourseName), { create: true });
    for (const dir of filePath) {
        const safe = sanitize(dir);
        if (!safe) continue;
        current = await current.getDirectoryHandle(safe, { create: true });
    }
    const safeName = sanitize(filename);
    const handle = await current.getFileHandle(safeName, { create: true });
    const writer = await handle.createWritable();
    await writer.write(bytes);
    await writer.close();
    console.log(`✅ Saved: ${[currentCourseName, ...filePath, safeName].join('/')} (${(bytes.length/1024).toFixed(1)} KB)`);
}

function sanitize(name) {
    return name.replace(/[<>:"/\\|?*\x00-\x1F]/g, '_').replace(/^\.+/, '_').replace(/\.$/, '_').trim().slice(0,200);
}

/* -------------------------------------------------------
   PAGE FETCHING
------------------------------------------------------- */
async function fetchPage(url) {
    console.log(`🌐 Fetching: ${url}`);
    const resp = await fetch(url, { credentials: 'include', headers: {'Accept': 'text/html,application/xhtml+xml'} });
    if (!resp.ok) throw new Error(`HTTP ${resp.status} for ${url}`);
    const html = await resp.text();
    return new DOMParser().parseFromString(html, 'text/html');
}

/* -------------------------------------------------------
   CRAWLING (Phase 1: collect URLs with paths)
------------------------------------------------------- */
async function crawl(url, path = []) {
    const key = getPageKey(url);
    if (visitedPages.has(key)) {
        console.log(`⏭️ Already visited: ${path.join(' > ') || 'Root'}`);
        return;
    }
    visitedPages.add(key);
    console.log(`📂 Crawling: ${path.join(' > ') || 'Root'}`);

    try {
        const doc = await fetchPage(url);
        const files = getFiles(doc, path);
        if (files.length) {
            console.log(`📎 Found ${files.length} files in: ${path.join(' > ') || 'Root'}`);
            allFiles.push(...files);
            totalFiles = allFiles.length;
        }
        const folders = getFolders(doc);
        for (const f of folders) {
            if (!isRunning) break;
            await sleep(CONFIG.REQUEST_DELAY);
            await crawl(f.url, [...path, f.name]);
        }
        // Pagination
        const next = doc.querySelector('a[title="Next"], a.pagination-next, .next a, a[aria-label="Next"]');
        if (next) {
            const nextUrl = new URL(next.href, location.origin).href;
            if (nextUrl !== url) {
                await sleep(CONFIG.REQUEST_DELAY);
                await crawl(nextUrl, path);
            }
        }
    } catch (e) {
        console.error(`❌ Crawl error: ${url}`, e);
    }
}

/* -------------------------------------------------------
   Phase 2: Download all collected files
------------------------------------------------------- */
async function downloadAllFiles() {
    console.log(`\n📥 Starting download of ${allFiles.length} files...`);
    for (let i = 0; i < allFiles.length; i += CONFIG.MAX_PARALLEL_DOWNLOADS) {
        if (!isRunning) break;
        const batch = allFiles.slice(i, i + CONFIG.MAX_PARALLEL_DOWNLOADS);
        await Promise.all(batch.map(async (file) => {
            try {
                if (seenUrls.has(file.url)) {
                    console.log(`⏭️ Skipping duplicate URL: ${file.filename}`);
                    processedFiles++;
                    updateUI({ total: totalFiles, processed: processedFiles, failed: failedFiles.length, status: 'running' });
                    return;
                }
                const { bytes, filename } = await downloadWithRetry(file.url);
                if (filename) file.filename = filename;
                file.filename = fixFileExtension(file.filename, bytes);
                if (CONFIG.SKIP_DUPLICATE_CONTENT) {
                    const fp = await fingerprint(bytes);
                    if (seenFingerprints.has(fp)) {
                        console.log(`⏭️ Skipping duplicate content: ${file.filename}`);
                        processedFiles++;
                        updateUI({ total: totalFiles, processed: processedFiles, failed: failedFiles.length, status: 'running' });
                        return;
                    }
                    seenFingerprints.add(fp);
                }
                seenUrls.add(file.url);
                file.filename = handleDuplicateFilename(file.filename);
                await saveFile(file.path, file.filename, bytes);
                processedFiles++;
                updateUI({ total: totalFiles, processed: processedFiles, failed: failedFiles.length, status: 'running' });
            } catch (e) {
                console.error(`❌ Failed: ${file.filename}`, e);
                failedFiles.push({...file, error: e.message});
                processedFiles++;
                updateUI({ total: totalFiles, processed: processedFiles, failed: failedFiles.length, status: 'running' });
            }
        }));
        if (i + CONFIG.MAX_PARALLEL_DOWNLOADS < allFiles.length) await sleep(CONFIG.REQUEST_DELAY);
    }
}

/* -------------------------------------------------------
   CLEANUP OLD DATA
------------------------------------------------------- */
async function clearCourseFolder() {
    const root = await navigator.storage.getDirectory();
    try {
        await root.removeEntry(sanitize(currentCourseName), { recursive: true });
        console.log('🧹 Cleared previous course folder');
    } catch (e) {
        // Directory doesn't exist, that's fine
    }
}

/* -------------------------------------------------------
   MAIN CONTROLLER
------------------------------------------------------- */
async function startDownload() {
    if (isRunning) return;
    isRunning = true;

    // Reset state
    visitedPages.clear();
    seenFingerprints.clear();
    seenUrls.clear();
    filenameCountMap.clear();
    allFiles = [];
    failedFiles = [];
    totalFiles = 0;
    processedFiles = 0;

    currentCourseId = extractCourseId();
    if (!currentCourseId) {
        alert('Could not find course ID. Navigate to the course main page.');
        isRunning = false;
        return;
    }

    updateUI({ status: 'running', total: 0, processed: 0, failed: 0 });

    try {
        // ---- STEP 0: Delete old course folder for a clean start ----
        if (CONFIG.CLEAR_PREVIOUS_DATA) {
            // Need course name first; we'll fetch the main page quickly to get it
            const classicUrl = `${location.origin}/webapps/blackboard/execute/courseMain?course_id=${currentCourseId}`;
            const doc = await fetchPage(classicUrl);
            currentCourseName = doc.querySelector("#courseMenu_link")?.textContent?.trim() ||
                               doc.querySelector("h1")?.textContent?.trim() || "Blackboard Course";
            await clearCourseFolder();
            // Re-create the course directory
            const root = await navigator.storage.getDirectory();
            await root.getDirectoryHandle(sanitize(currentCourseName), { create: true });
        }

        // ---- Phase 1: Crawl ----
        console.log('🔍 Phase 1: Crawling course structure...');
        // If we didn't already fetch the main page for cleaning, fetch it now
        if (!currentCourseName) {
            const classicUrl = `${location.origin}/webapps/blackboard/execute/courseMain?course_id=${currentCourseId}`;
            const doc = await fetchPage(classicUrl);
            currentCourseName = doc.querySelector("#courseMenu_link")?.textContent?.trim() ||
                               doc.querySelector("h1")?.textContent?.trim() || "Blackboard Course";
            // Ensure course folder exists
            const root = await navigator.storage.getDirectory();
            await root.getDirectoryHandle(sanitize(currentCourseName), { create: true });
        }
        const sections = getCourseSections(await fetchPage(`${location.origin}/webapps/blackboard/execute/courseMain?course_id=${currentCourseId}`));
        console.log(`📋 Found ${sections.length} sections`);
        for (const section of sections) {
            if (!isRunning) break;
            await crawl(section.url, [section.name]);
            await sleep(CONFIG.REQUEST_DELAY);
        }
        console.log(`📊 Phase 1 complete: Found ${allFiles.length} files`);

        // ---- Phase 2: Download ----
        console.log('📥 Phase 2: Downloading files...');
        updateUI({ total: allFiles.length, processed: 0, failed: 0, status: 'running' });
        await downloadAllFiles();

        const success = processedFiles - failedFiles.length;
        console.log(`✅ Complete! Downloaded: ${success}, Failed: ${failedFiles.length}`);
        updateUI({ total: totalFiles, processed: success, failed: failedFiles.length, status: 'complete' });
    } catch (e) {
        console.error('❌ Fatal error:', e);
        updateUI({ total: totalFiles, processed: processedFiles, failed: failedFiles.length, status: 'error' });
    } finally {
        isRunning = false;
    }
}

function cancelDownload() {
    isRunning = false;
    console.log('⏹️ Cancelled');
    updateUI({ total: totalFiles, processed: processedFiles, failed: failedFiles.length, status: 'cancelled' });
}

function extractCourseId() {
    const pathMatch = location.pathname.match(/courses\/([^\/]+)/);
    if (pathMatch) return pathMatch[1];
    const params = new URLSearchParams(location.search);
    const id = params.get('course_id');
    if (id) return id;
    const links = document.querySelectorAll('a[href*="course_id="]');
    for (const l of links) {
        const m = l.href.match(/course_id=([^&]+)/);
        if (m) return m[1];
    }
    return null;
}

/* -------------------------------------------------------
   ZIP EXPORT
------------------------------------------------------- */
async function exportZip() {
    try {
        const root = await navigator.storage.getDirectory();
        let courseDir;
        try { courseDir = await root.getDirectoryHandle(sanitize(currentCourseName)); }
        catch { alert('No downloaded course found'); return; }
        console.log('🔍 Collecting files...');
        const data = await getAllFilesRecursive(courseDir);
        console.log(`📦 Creating ZIP with ${Object.keys(data).length} files`);
        fflate.zip(data, { level: 0 }, (err, bytes) => {
            if (err) { console.error(err); alert('ZIP failed'); return; }
            const blob = new Blob([bytes], {type: 'application/zip'});
            const url = URL.createObjectURL(blob);
            GM_download({ url, name: sanitize(currentCourseName)+'.zip', onload: () => URL.revokeObjectURL(url) });
            console.log('✅ ZIP export complete');
        });
    } catch (e) { console.error(e); alert('Export failed'); }
}

async function getAllFilesRecursive(dir, path = '') {
    const result = {};
    for await (const [name, handle] of dir.entries()) {
        const fullPath = path ? `${path}/${name}` : name;
        if (handle.kind === 'directory') Object.assign(result, await getAllFilesRecursive(handle, fullPath));
        else {
            const file = await handle.getFile();
            result[fullPath] = new Uint8Array(await file.arrayBuffer());
        }
    }
    return result;
}

function sleep(ms) { return new Promise(r => setTimeout(r, ms)); }

/* -------------------------------------------------------
   INIT
------------------------------------------------------- */
addEnhancedUI();
console.log('🚀 Blackboard Full Downloader v5.0 ready – auto-cleanup enabled');

})();