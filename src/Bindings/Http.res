type http
@module("http") external create: Express.express => http = "Server"
@send external listen: (http, int, unit => unit) => unit = "listen"
