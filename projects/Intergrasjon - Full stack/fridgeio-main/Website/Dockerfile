FROM nginx

WORKDIR /usr/share/react

RUN curl -fsSL https://deb.nodesource.com/setup_17.x | bash -
RUN apt-get install -y nodejs
RUN apt-get install -y npm

COPY package*.json ./

COPY nginx.conf /etc/nginx/conf.d/default.conf

RUN npm install

RUN npm install -g env-cmd

COPY . .

RUN npm run build-dev

RUN rm -r /usr/share/nginx/html/*

RUN cp -a build/. /usr/share/nginx/html

EXPOSE 80
