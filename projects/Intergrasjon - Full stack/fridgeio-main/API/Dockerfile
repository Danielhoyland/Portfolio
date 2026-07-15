FROM golang:1.19
LABEL maintainer="odinaa@stud.ntnu.no"

COPY cmd.go /app/cmd.go
COPY ./handlers /app/handlers
COPY ./database /app/database
COPY fridgeio.db /app/fridgeio.db
COPY go.mod /app/go.mod
COPY go.sum /app/go.sum

WORKDIR /app

RUN CGO_ENABLED=1 GOOS=linux go build -a -ldflags '-extldflags "-static"' -o main

EXPOSE 8080

CMD ["./main"]