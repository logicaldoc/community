<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ page import="javax.servlet.*" %>
<%@ page import="javax.servlet.http.*" %>
<%
  String docserviceApiUrl = (String)request.getAttribute("docserviceApiUrl"); 
  String configJSON = (String)request.getAttribute("config"); 
%>	
<html>
<head>
<script type="text/javascript" src="<%=docserviceApiUrl%>"></script>
<script type="text/javascript" language="javascript">

		var docEditor;
        var config;

        var innerAlert = function (message, inEditor) {
            if (console && console.log)
                console.log(message);
            if (inEditor && docEditor)
                docEditor.showMessage(message);
        };

        // the application is loaded into the browser
        var onAppReady = function () {
            innerAlert("Document editor ready");
        };

        // the document is modified
        var onDocumentStateChange = function (event) {
            var title = document.title.replace(/\*$/g, "");
            document.title = title + (event.data ? "*" : "");
        };        

        // the user is trying to switch the document from the viewing into the editing mode
        var onRequestEditRights = function () {
            location.href = location.href.replace(RegExp("mode=\\w+\&?", "i"), "") + "&mode=edit";
        };
		
        // an error or some other specific event occurs
        var onError = function (event) {
            if (event)
                innerAlert(event.data);
        };

        // the document is opened for editing with the old document.key value
        var onOutdatedVersion = function (event) {
            location.reload(true);
        };
		
        // replace the link to the document which contains a bookmark
        var replaceActionLink = function(href, linkParam) {
            var link;
            var actionIndex = href.indexOf("&actionLink=");
            if (actionIndex != -1) {
                var endIndex = href.indexOf("&", actionIndex + "&actionLink=".length);
                if (endIndex != -1) {
                    link = href.substring(0, actionIndex) + href.substring(endIndex) + "&actionLink=" + encodeURIComponent(linkParam);
                } else {
                    link = href.substring(0, actionIndex) + "&actionLink=" + encodeURIComponent(linkParam);
                }
            } else {
                link = href + "&actionLink=" + encodeURIComponent(linkParam);
            }
            return link;
        }

        // the user is trying to get link for opening the document which contains a bookmark, scrolling to the bookmark position
        var onMakeActionLink = function (event) {
            var actionData = event.data;
            var linkParam = JSON.stringify(actionData);
            docEditor.setActionLink(replaceActionLink(location.href, linkParam));  // set the link to the document which contains a bookmark
        };

        // the meta information of the document is changed via the meta command
        var onMetaChange = function (event) {
            if (event.data.favorite) {
                var favorite = !!event.data.favorite;
                var title = document.title.replace(/^\☆/g, "");
                document.title = (favorite ? "☆" : "") + title;
                docEditor.setFavorite(favorite);  // change the Favorite icon state
            }

            innerAlert("onMetaChange: " + JSON.stringify(event.data));
        };
		
        var onRequestSaveAs = function (event) {  //  the user is trying to save file by clicking Save Copy as... button
            var title = event.data.title;
            var url = event.data.url;
            var data = {
                title: title,
                url: url
            };
            let xhr = new XMLHttpRequest();
            xhr.open("POST", "/logicaldoc/onlyoffice/IndexServlet?type=saveas");
            xhr.setRequestHeader('Content-Type', 'application/json');
            xhr.send(JSON.stringify(data));
            xhr.onload = function () {
                innerAlert(xhr.responseText);
                innerAlert(JSON.parse(xhr.responseText).file, true);
            }
        };

        // Doesn't work - Alex 22/08/2024
        var onRequestRename = function(event) { //  the user is trying to rename file by clicking Rename... button
            innerAlert("onRequestRename: " + JSON.stringify(event.data));

            var newfilename = event.data;
            var data = {
                newfilename: newfilename,
                dockey: config.document.key,
                ext: config.document.fileType
            };
            let xhr = new XMLHttpRequest();
            xhr.open("POST", "/logicaldoc/onlyoffice/IndexServlet?type=rename");
            xhr.setRequestHeader('Content-Type', 'application/json');
            xhr.send(JSON.stringify(data));
            xhr.onload = function () {
                innerAlert(xhr.responseText);
            }
        };

        var onRequestOpen = function(event) {  // user open external data source
            innerAlert("onRequestOpen");
            var windowName = event.data.windowName;

            requestReference(event.data, function (data) {
                if (data.error) {
                    var winEditor = window.open("", windowName);
                    winEditor.close();
                    innerAlert(data.error, true);
                    return;
                }

                var link = data.link;
                window.open(link, windowName);
            });
        };

        var onRequestReferenceData = function(event) {  // user refresh external data source
            innerAlert("onRequestReferenceData");
            requestReference(event.data, function (data) {
                docEditor.setReferenceData(data);
            });
        };

        var requestReference = function(data, callback) {
            innerAlert(data);
            data.directUrl = !!config.document.directUrl;

            let xhr = new XMLHttpRequest();
            xhr.open("POST", "/logicaldoc/onlyoffice/IndexServlet?type=reference");
            xhr.setRequestHeader("Content-Type", "application/json");
            xhr.send(JSON.stringify(data));
            xhr.onload = function () {
                innerAlert(xhr.responseText);
                callback(JSON.parse(xhr.responseText));
            }
        };

        function onRequestRestore(event) {
          const query = new URLSearchParams(window.location.search)
          const payload = {
            fileName: query.get('fileName'),
            version: event.data.version,
            userId: config.editorConfig.user.id
          }
          const request = new XMLHttpRequest()
          request.open('PUT', '/logicaldoc/onlyoffice/IndexServlet?type=restore')
          request.send(JSON.stringify(payload))
          request.onload = function () {
            const response = JSON.parse(request.responseText);
            if (response.success && !response.error) {
              var historyInfoUri = "/onlyoffice/IndexServlet?type=history&filename=" + config.document.title;
              var xhr = new XMLHttpRequest();
              xhr.open("GET", historyInfoUri, false);
              xhr.send();

              if (xhr.status == 200) {
                  var historyInfo = JSON.parse(xhr.responseText);
                  docEditor.refreshHistory(historyInfo);
              }
            } else {
              innerAlert(response.error);
            }
          }
        }

        var onRequestHistory = function () {
            var historyInfoUri = "/logicaldoc/onlyoffice/IndexServlet?type=history&filename=" + config.document.title;
            var xhr = new XMLHttpRequest();
            xhr.open("GET", historyInfoUri, false);
            xhr.send();

            if (xhr.status == 200) {
                var historyInfo = JSON.parse(xhr.responseText);
                docEditor.refreshHistory(historyInfo);
            }
        };

        var onRequestHistoryData = function (event) {
            var version = event.data;
            var historyDataUri = "/logicaldoc/onlyoffice/IndexServlet?type=historyData&filename=" + config.document.title
                + "&version=" + version
                + "&directUrl=" + !!config.document.directUrl;
            var xhr = new XMLHttpRequest();
            xhr.open("GET", historyDataUri, false);
            xhr.send();

            if (xhr.status == 200) {
                var historyData = JSON.parse(xhr.responseText);
                docEditor.setHistoryData(historyData);
            }
        };

        var onRequestHistoryClose = function() {
            document.location.reload();
        };		

		config = JSON.parse('<%=configJSON%>');
		config.width = "100%";
		config.height = "100%";
		config.width = "100%";
        config.events = {
            "onAppReady": onAppReady,
            "onDocumentStateChange": onDocumentStateChange,
            "onError": onError,
            "onOutdatedVersion": onOutdatedVersion,
            "onMakeActionLink": onMakeActionLink,
            "onMetaChange": onMetaChange,
            "onRequestRestore": onRequestRestore,
            "onRequestHistory": onRequestHistory,
            "onRequestHistoryData": onRequestHistoryData,
            "onRequestHistoryClose": onRequestHistoryClose
        };
		
        <%
            String usersForMentions = (String) request.getAttribute("usersForMentions");
            String usersInfo = (String) request.getAttribute("usersInfo");
            String usersForProtect = (String) request.getAttribute("usersForProtect");
        %>

        if (config.editorConfig.user.id) {
            // add mentions for not anonymous users
            config.events['onRequestUsers'] = function (event) {
                if (event && event.data){
                    var c = event.data.c;
                }
                switch (c) {
                    case "info":
                        users = [];
                        var allUsers = <%=usersInfo%>;
                        for (var i = 0; i < event.data.id.length; i++) {
                            for (var j = 0; j < allUsers.length; j++) {
                                if (allUsers[j].id == event.data.id[i]) {
                                    users.push(allUsers[j]);
                                    break;
                                }
                            }
                        }
                        break;
                    case "protect":
                        var users = <%=usersForProtect%>;
                        break;
                    default:
                        users = <%=usersForMentions%>;
                }
                docEditor.setUsers({
                    "c": c,
                    "users": users,
                });
            };
            // the user is mentioned in a comment
            config.events['onRequestSendNotify'] = function (event) {
                event.data.actionLink = replaceActionLink(location.href, JSON.stringify(event.data.actionLink));
                var data = JSON.stringify(event.data);
                innerAlert("onRequestSendNotify: " + data);
            };
            // prevent file renaming for anonymous users
            config.events['onRequestRename'] = onRequestRename;
            config.events['onRequestReferenceData'] = onRequestReferenceData;
            // prevent switch the document from the viewing into the editing mode for anonymous users
            config.events['onRequestEditRights'] = onRequestEditRights;
            config.events['onRequestOpen'] = onRequestOpen;
        }		

        if (config.editorConfig.createUrl) {
            config.events.onRequestSaveAs = onRequestSaveAs;
        };

        var сonnectEditor = function () {
            if ((config.document.fileType === "docxf" || config.document.fileType === "oform")
                && DocsAPI.DocEditor.version().split(".")[0] < 7) {
                innerAlert("Please update ONLYOFFICE Docs to version 7.0 to work on fillable forms online.");
                return;
            }

            docEditor = new DocsAPI.DocEditor("iframeEditor", config);
        };

        if (window.addEventListener) {
            window.addEventListener("load", сonnectEditor);
        } else if (window.attachEvent) {
            window.attachEvent("load", сonnectEditor);
        }
</script>
</head>
    <body>
        <div class="form" style="margin-top:20px;padding-top:20px">
            <div id="iframeEditor"></div>
        </div>
    </body>
</html>