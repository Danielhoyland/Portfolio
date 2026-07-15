FROM nginx

WORKDIR /usr/share/react

RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash -
RUN apt-get install -y nodejs

COPY package*.json ./

RUN npm install

COPY . .

COPY .env-cmdrc.json ./.env-cmdrc

RUN npm run serveServer

RUN rm -r /usr/share/nginx/html/*

RUN cp -a dist/. /usr/share/nginx/html

COPY nginx.conf /etc/nginx/nginx.conf


EXPOSE 80