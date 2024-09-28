FROM golang:1.19
LABEL maintainer="odinaa@stud.ntnu.no"

COPY ./cmd /go/src/app/cmd
COPY ./handler /go/src/app/handler
COPY ./helper /go/src/app/helper
COPY ./go.mod /go/src/app/go.mod
COPY ./owid-energy-data.csv /go/src/app/cmd/owid-energy-data.csv
COPY ./owid-energy-data.csv /go/src/app/owid-energy-data.csv

WORKDIR /go/src/app/cmd

RUN CGO_ENABLED=0 GOOS=linux go build -a -ldflags '-extldflags "-static"' -o main

EXPOSE 8080


CMD ["./main"]

