#!/usr/bin/env python
# coding: utf-8

# 
# # Assignment 4: Embedding Models, Dense Retrieval, and RAG
# 
# **Student names**: Daniel Høyland <br>
# **Group number**: 4 <br>
# **Date**: 22.10.2025
# 
# ## Important notes
# Please carefully read the following notes and consider them for the assignment delivery. Submissions that do not fulfill these requirements will not be assessed and should be submitted again.
# 1. You may work in groups of maximum 2 students.
# 2. The assignment must be delivered in ipynb format.
# 3. The assignment must be typed. Handwritten assignments are not accepted.
# 
# **Due date**: 26.10.2025 23:59
# 
# In this assignment, you will:
# - Build a vector search index over a blog corpus using sentence embeddings
# - Implement dense retrieval (cosine similarity)
# - Use the vector index as the foundation for a simple Retrieval-Augmented Generation (RAG) chat system with evaluation on three queries
# 

# 
# ---
# ## Dataset
# 
# You will use the blog files, provided in the folder: 
# - `blogs-sample` (in the same directory as this notebook)
# 
# Use only the blog files provided in the folder below. Each file contains multiple `<post>` elements. Treat **each `<post>` as a separate document**.
# 
# **The code to parse files is not provided. Implement the loading yourself in 4.1.**
# 

# 
# ## 4.1 – Load and parse blog documents
# 
# Load all XML files from `blogs-sample`, extract the text of each `<post>`, and store one string per document. Keep the raw text per post as the document text.
# 
# You may experience some trouble parsing all lines in the files, but this is okay.
# 
# 

# In[4]:


# TODO: Load and parse the blog posts into a list named `documents`.

# Your code here
import os
import re
import xml.etree.ElementTree as ET

folder_path = "blogs-sample"
documents = []

def extract_posts_fallback(file_path):
    """Fallback: Extract <post>...</post> manually if XML parsing fails."""
    posts = []
    try:
        with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
            content = f.read()
            raw_posts = re.findall(r"<post>(.*?)</post>", content, re.DOTALL)
            for p in raw_posts:
                p_clean = p.strip()
                if p_clean:
                    posts.append(p_clean)
    except Exception as e:
        print(f"Could not read {file_path}: {e}")
    return posts


for filename in os.listdir(folder_path):
    if filename.endswith(".xml"):
        file_path = os.path.join(folder_path, filename)
        try:
            tree = ET.parse(file_path)
            root = tree.getroot()
            for post in root.findall(".//post"):
                text = post.text
                if text:
                    documents.append(text.strip())
        except ET.ParseError:
            recovered_posts = extract_posts_fallback(file_path)
            documents.extend(recovered_posts)
            print(f"Recovered {len(recovered_posts)} posts from {filename} via fallback.")
        except Exception as e:
            print(f"Error reading {filename}: {e}")

print(f"✅ Loaded {len(documents)} documents total.")


# 
# ## 4.2 – Embedding Models
# 
# Select and load a sentence embedding model (e.g., `sentence-transformers/all-MiniLM-L6-v2`) and compute embeddings for all documents.
# 
# - Store document embeddings in a variable named `doc_embeddings`.
# - Ensure that the same model will be used for query encoding later.
# 
# **Report**:
# - The embedding matrix shape 
# 

# In[8]:


# TODO: Load a sentence embedding model and encode all documents into `doc_embeddings`.
# You may use `sentence-transformers`. Report the embedding matrix shape.

# Your code here
from sentence_transformers import SentenceTransformer
import numpy as np

model_name = "sentence-transformers/all-MiniLM-L6-v2"
model = SentenceTransformer(model_name)

doc_embeddings = model.encode(documents, show_progress_bar=True, convert_to_numpy=True)

print(f"Document embeddings shape: {doc_embeddings.shape}")


# 
# ## 4.3 – Dense Retrieval
# 
# Implement a cosine similarity search over `doc_embeddings` for a given query.
# 
# - Write a function `dense_search(query: str, k: int = 5) -> list[int]` that returns the indices of the top-k documents.
# - Use the same embedding model to encode the query.
# - Use cosine similarity for ranking.
# 
# **Report**:
# - Results for the provided query showing the indices of the top results.
# 

# In[9]:


# TODO: Implement dense retrieval using cosine similarity.
# Function signature to implement:
# def dense_search(query: str, k: int = 5) -> list[int]:

# Your code here
from sklearn.metrics.pairwise import cosine_similarity

def dense_search(query: str, k: int = 5) -> list[int]:
    query_embedding = model.encode([query], convert_to_numpy=True)

    similarities = cosine_similarity(query_embedding, doc_embeddings)[0]

    top_k_idx = similarities.argsort()[::-1][:k]
    return top_k_idx.tolist()

print(dense_search("How do people feel about their jobs?", k=5))



# 
# ## 4.4 – Build a Vector Search Index
# 
# Build a lightweight vector index structure to enable repeated querying efficiently.
# 
# - You may reuse `doc_embeddings` directly or create an index structure. Ensure the index can return top-k document indices given a query vector.
# 

# In[10]:


# TODO: Initialize a vector index over `doc_embeddings`
# Keep code minimal. The goal is to enable fast top-k retrieval for repeated queries.

# Your code here
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np

class VectorIndex:
    def __init__(self, embeddings: np.ndarray):
        """
        Initialize the vector index with precomputed embeddings.
        """
        self.embeddings = embeddings
        self.norms = np.linalg.norm(embeddings, axis=1, keepdims=True)

    def search(self, query_embedding: np.ndarray, k: int = 5) -> list[int]:
        """
        Return the indices of top-k documents given a query embedding.
        """
        query_norm = query_embedding / np.linalg.norm(query_embedding)
        similarities = np.dot(self.embeddings, query_norm.T).flatten()
        top_k_idx = similarities.argsort()[::-1][:k]
        return top_k_idx.tolist()

vector_index = VectorIndex(doc_embeddings)

query = "How do people feel about their jobs?"
query_embedding = model.encode([query], convert_to_numpy=True)
top_docs = vector_index.search(query_embedding, k=5)
print("Top-5 document indices:", top_docs)


# 
# ## 4.5 – RAG (Retrieval-Augmented Generation)
# 
# Implement a simple RAG pipeline that:
# 1) Retrieves the top-k documents for a user query using your vector index.
# 2) Builds a prompt that includes the query and the retrieved document snippets.
# 3) Uses a text generation model (your choice) to produce an answer grounded in the retrieved snippets.
# 
# - Implement a function `rag_answer(query: str, k: int = 5) -> str`.
# - Keep the prompt simple and state clearly that the model should rely on the provided context.
# 

# In[39]:


embedding_model = SentenceTransformer("sentence-transformers/all-MiniLM-L6-v2")

from transformers import AutoTokenizer, AutoModelForSeq2SeqLM, pipeline

gen_model_name = "google/flan-t5-base"
gen_tokenizer = AutoTokenizer.from_pretrained(gen_model_name)
gen_model = AutoModelForSeq2SeqLM.from_pretrained(gen_model_name)
generator = pipeline("text2text-generation", model=gen_model, tokenizer=gen_tokenizer)

def rag_answer(query: str, k: int = 5) -> str:
    """
    Improved RAG pipeline for realistic and coherent answers.
    Cleans, trims, and summarizes context before generation.
    """
    query_embedding = embedding_model.encode([query], convert_to_numpy=True)
    top_indices = vector_index.search(query_embedding, k=k)

    context_snippets = []
    for idx in top_indices:
        snippet = documents[idx]
        snippet = re.sub(r"<.*?>", " ", snippet)
        snippet = re.sub(r"\s+", " ", snippet).strip()
        if 40 < len(snippet) < 1000:
            context_snippets.append(snippet)
    context = "\n\n---\n\n".join(context_snippets[:k])

    prompt = (
        "You are summarizing real blog posts written by ordinary people. "
        "Use the CONTEXT to answer the QUESTION truthfully and realistically.\n\n"
        "Focus on how people *feel* and what attitudes they express. "
        "Avoid quoting text directly; summarize in your own words.\n"
        "Write naturally, as if you’re describing what multiple bloggers said.\n"
        "If the context gives mixed opinions, mention that.\n\n"
        f"CONTEXT:\n{context}\n\n"
        f"QUESTION: {query}\n\n"
        "Answer (2–4 sentences):"
    )

    inputs = gen_tokenizer(
        prompt,
        return_tensors="pt",
        truncation=True,
        max_length=512,
    )

    result = gen_model.generate(
        **inputs,
        max_new_tokens=160,
        do_sample=False,
        temperature=0.2,
        top_p=0.9,
        top_k=50,
    )

    answer = gen_tokenizer.decode(result[0], skip_special_tokens=True).strip()
    return answer


query = "How do people feel about their jobs?"
answer = rag_answer(query, k=5)
print("RAG Answer:\n", answer)


# ## 4.6 – Evaluation
# 
# Use the following queries for your evaluation. For each query:
# 
# - Run `dense_search(query, k=5)` to retrieve relevant documents.
# - Use `rag_answer(query, k=5)` to generate an answer using the top-5 retrieved documents.
# 
# **Queries:**
# 1. How do people deal with breakups?
# 2. What do bloggers write about their daily routines?
# 3. How do people feel about their jobs?
# 

# In[40]:


# Do not change this code
queries = [
    "How do people deal with breakups?",
    "What do bloggers write about their daily routines?",
    "How do people feel about their jobs?"
]


# In[41]:


# TODO: Run and report your evaluation as described above.

def run_batch_evaluation(queries, k=5):
    for i, query in enumerate(queries, 1):
        print("=" * 100)
        print(f"Q{i}: {query}")
        print("-" * 100)

        top_k = dense_search(query, k=k)
        print(f"Top-{k} retrieved indices:", top_k)
        print("\nTop retrieved snippets:")
        for idx in top_k:
            snippet = documents[idx].replace("\n", " ").strip()
            print(f"[{idx}] {snippet[:200]}...\n")

        print("RAG answer:\n")
        answer = rag_answer(query, k=k)
        print(answer)
        print("\n")

run_batch_evaluation(queries, k=5)


# In[ ]:




