<%-- 
   Page Displayed when a release change is detected
--%>
<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@page import="javax.servlet.http.Cookie"%>
<%@page import="java.io.*"%>
<%@page import="java.util.*"%>
<%@page import="com.logicaldoc.util.config.*"%>
<%!
    public String getClientVersion(HttpServletRequest request){
      return getCookie("ldoc-version",request);
    }

    public String getServerVersion(){
      return getProperty("product.release");
    }
    
    public String getProperty(String name){
      try{
        ContextProperties config=new ContextProperties();
        return config.getProperty(name);
      }catch(Throwable t){
      }  
      return null;
    }
    
    public String getCookie(String name, HttpServletRequest request){
      Cookie[] cookielist = request.getCookies();
      if(cookielist != null){
        for(int i = 0; i < cookielist.length;i++)
          if(name.equals(cookielist[i].getName()))
            return cookielist[i].getValue();
      }      
      return null;
    }
    
    public void deleteCookie(String name, HttpServletResponse response){
      Cookie killMyCookie = new Cookie(name, null);
      killMyCookie.setMaxAge(0);
      killMyCookie.setPath("/");
      response.addCookie(killMyCookie);
    }
    
    public String message(String message, HttpServletRequest request){
        try{
          ResourceBundle res = ResourceBundle.getBundle("i18n.messages", request.getLocale());
          return res.getString(message);
        }catch(Throwable t){
        }
        return message;
    }
%>
<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<!-- UP AND RUNNING -->
<html xmlns="http://www.w3.org/1999/xhtml">
	<head>
    <!--meta http-equiv="X-UA-Compatible" content="IE=EmulateIE10" /-->
    <meta name="robots" content="noindex">
		<title></title>
		<!--CSS for loading message at application Startup-->
		<style type="text/css">
body {
	overflow: hidden
}

#loading {
	border: 1px solid #ccc;
	position: absolute;
	left: 45%;
	top: 40%;
	padding: 2px;
	z-index: 20001;
	height: auto;
}

#loading a {
	color: #225588;
}

#loading .loadingIndicator {
	background: white;
	font: bold 13px tahoma, arial, helvetica;
	padding: 10px;
	margin: 0;
	height: auto;
	color: #444;
}

#loadingMsg {
	font: normal 10px arial, tahoma, sans-serif;
}
</style>

<link REL="STYLESHEET" HREF="./skin/style.css" TYPE="text/css" />
<link id="favicon" rel="shortcut icon" type="image/png" href='' />
</head>
<body>
    <p>&nbsp;</p>
    <p>
       <%=message("newrelseasewarning",request)%>
    </p>
    <p>
       <%=message("performfollowingsteps",request)%>:
       <ol>
         <li><%=message("deletecache",request)%> <a href="http://docs.logicaldoc.com/en/update/clear-browser-cache" target="_blank"><%=message("clickforhowto",request)%></a></li>
         <li><%=message("loginfromhere",request)%>: <a href="${pageContext.request.contextPath}/${param.loginpage}?skipreleasecheck=true"><%=message("login",request)%></a></li>
       </ol>
    </p>
    <hr/>
    
    <p>
      <p style="font-size: small">
        client version: <b><%= getClientVersion(request) %></b><br/>
        server version: <b><%= getServerVersion() %></b>
      </p>
    </p>
</body>
</html>