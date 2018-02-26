/*
  @license MIT
  server.js
*/

var Express = require("express");
var Http = require("http");
var Https = require("https");
var Path = require("path");
var WebSocketServer = require("ws").Server;

var app = Express();
app.use(Express.static(Path.join(__dirname, "./dist")));

var server = Http.createServer();
var wsServer = new WebSocketServer({ server : server });

var clientMap = { };

wsServer.on("connection", onConnection);

function getContact(cid)
{
  var contact =
    { cid : cid
    , name : clientMap[cid].name.first + " " + clientMap[cid].name.last
    , picture : clientMap[cid].picture
    };

  return contact;
}

function onResponse(responseCallback)
{
  return function (response)
  {
    var body = "";
    response.on("data", chunk => body += chunk);
    response.on("end", () => responseCallback(body));
  }
}

function onContactListChanged()
{
  var contactList = [ ];

  for (var cid in clientMap)
  {
    contactList.push(getContact(cid));
  }

  for (var cid in clientMap)
  {
    var message =
      { _type : "contactList"
      , body : contactList.filter(contact => contact.cid !== cid)
      };

    clientMap[cid].client.send(JSON.stringify(message));
  }
}

function onClose(cid)
{
  return function()
  {
    delete clientMap[cid];
    onContactListChanged();
  }
}

function onConnection(client)
{
  function onNewUser(strUser)
  {
    var apiUser = JSON.parse(strUser).results[0];
    var cid = apiUser.login.username;

    client.on("message", onMessage(cid));
    client.on("close", onClose(cid));

    clientMap[cid] =
      { client : client
      , name : apiUser.name
      , picture : apiUser.picture.medium
      };

    client.send(JSON.stringify({ _type : "login", body : getContact(cid) }));
    onContactListChanged();
  }

  var getUserPath = { host: "randomuser.me", path: "/api" };
  Https.get(getUserPath, onResponse(onNewUser));
}

function onMessage(cid)
{
  return function (strMessage)
  {
    var message = JSON.parse(strMessage);

    if (!clientMap[cid])
    {
      return;
    }

    clientMap[cid].client.send(strMessage);
    clientMap[message.body.to].client.send(strMessage);
  }
}

server.on("request", app);
server.listen(3000, () => console.log("Listening on http://localhost:3000"));
