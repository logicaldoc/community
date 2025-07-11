<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@page import="jakarta.servlet.http.Cookie"%>
<%@page import="java.io.*"%>
<%@page import="java.util.*"%>
<%@page import="com.logicaldoc.util.spring.*"%>
<%@page import="com.logicaldoc.util.*"%>
<%@page import="com.logicaldoc.util.config.*"%>
<%@page import="com.logicaldoc.core.security.*"%>
<%@page import="com.logicaldoc.core.security.dao.*"%>
<%@page import="com.logicaldoc.web.util.*"%>
<%!
    public String getClientVersion(HttpServletRequest request){
      return getCookie("ldoc-version",request);
    }

    public String getServerVersion(){
      return getProperty("product.release");
    }
    
    public String getProperty(String name){
      try{
        ContextProperties config=Context.get().getProperties();
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
    
    
    public String getSkin(String tenantName, HttpServletRequest request){
        return "Tahoe";
    }
    
    public String getTenant(HttpServletRequest request){
      	return "default";
    }
%>
<%
  String TENANT = getTenant(request);
  
  String SKIN = getSkin(TENANT, request);
  
  String locale = request.getParameter("locale");
	if (locale == null || "".equals(locale))
		locale = "";
    
	String dir="ltr";
	if (locale.startsWith("ar") || locale.startsWith("dv") || locale.startsWith("ha") || locale.startsWith("he")
				|| locale.startsWith("fa") || locale.startsWith("ps") || locale.startsWith("ur")
				|| locale.startsWith("yi"))
		dir="rtl";
    
  String sVersion=getServerVersion();
  String cVersion=getClientVersion(request);
  boolean versionChanged=cVersion!=null && !cVersion.equals(sVersion);
  
  if(versionChanged && !"releasecheck".equals(MODULE) && !"true".equals(request.getParameter("skipreleasecheck")))
     response.sendRedirect("./releasecheck.jsp?loginpage="+LOGIN_PAGE);
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

<link href="./fontawesome/css/all.min.css" rel="stylesheet">

<link id="favicon" rel="shortcut icon" type="image/png" href='' />
<script type="text/javascript">		
	// Determine what skin file to load
  var currentSkin = "<%=SKIN%>";
  var isomorphicDir = "<%=MODULE%>/sc/";
  var tenant = "<%=TENANT%>";
  var module = "<%=MODULE%>";
</script>

<script src="./js/html2canvas.min.js"></script>
<script type="text/javascript">
  function screenshot(id, title, printLabel, closeLabel) {
    html2canvas(document.querySelector("[eventproxy='"+id+"']")).then(canvas => {
        const dataUrl = canvas.toDataURL();       
       
        let windowContent = '<!DOCTYPE html> ';
        windowContent += '<html> ';
    
		    windowContent += '<head><title>' + title + '</title> ';
        windowContent += '\u003Cstyle> ';
        windowContent += '.cell, .cellDarkAltCol, .cellDark{white-space: nowrap;} ';
        windowContent += '.printHeader{white-space: nowrap; font-weight: bold; border:0px solid white;} ';
        windowContent += '\u003C/style> ';

        windowContent += "\u003Cscript type='text/javascript'> ";
        windowContent += "  function printPage(){document.getElementById('printPanel').style.display='none'; window.print(); window.close();} ";
        windowContent += "\u003C/script> ";
        
        windowContent += '</head> ';
    
        windowContent += '<body> ';
        
        windowContent += "<div id='printPanel' class='printPanel default'><ul><li><a href='javascript:printPage();' id='printButton'> ";
        windowContent += printLabel + "</a></li><li><a href='javascript:window.close();' id='printClose'> ";
        windowContent += closeLabel + "</a></li></ul></div> ";
        
        windowContent += "<img src='" + dataUrl + "'> ";
        
        windowContent += '</body>';
        windowContent += '</html>';

        const printWin = window.open('', title, 'directories=0,titlebar=0,toolbar=0,location=0,status=0,menubar=0,scrollbars=no,resizable=no,width=' + screen.availWidth + ',height=' + screen.availHeight);
        printWin.document.open();
        printWin.document.write(windowContent);
    });
  }
</script>

<script type="text/javascript">
function copy(text) {
            var textarea = document.createElement("textarea");
            textarea.textContent = text;
            textarea.style.position = "fixed";
            textarea.style.width = '2em';
            textarea.style.height = '2em';
            textarea.style.padding = 0;
            textarea.style.border = 'none';
            textarea.style.outline = 'none';
            textarea.style.boxShadow = 'none';
            textarea.style.background = 'transparent';
            document.body.appendChild(textarea);
            textarea.focus();
            textarea.select();
            try {
                document.execCommand("copy");
                document.body.removeChild(textarea);
                resolve();
            } catch (e) {

            }
}
</script>

<script src="./fontawesome/js/all.min.js"></script>

</head>