<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%@ page import="javax.servlet.http.*" %>
<%@ page import="java.util.*" %>
<%@ page import="com.logicaldoc.core.security.*" %>
<%! static String MODULE="login"; %>
<%! static String LOGIN_PAGE="login.jsp"; %>
<%@ include file="header.jsp" %>
<%@ include file="detectmobile.jsp" %>

<script type="text/javascript">
	var j_loginurl='${pageContext.request.contextPath}/<%=LOGIN_PAGE%>';
	var j_failureurl=j_loginurl;
	var j_layout='desktop';
    
<%
  if(request.getParameter("switch") != null && request.getSession(false)!=null)
    request.getSession(false).removeAttribute("RequestedUrl");
%>
    
<%
  if(request.getSession(false) != null && request.getSession(false).getAttribute("RequestedUrl")!=null 
     && !request.getSession(false).getAttribute("RequestedUrl").toString().contains("/ace/")
     && !request.getSession(false).getAttribute("RequestedUrl").toString().contains("/sc/")
     && !request.getSession(false).getAttribute("RequestedUrl").toString().contains("/info")
     && !request.getSession(false).getAttribute("RequestedUrl").toString().endsWith("/frontend")) {
%>
    var j_successurl='<%=request.getSession(false).getAttribute("RequestedUrl")%>';
<%
  } else {
%>
    var j_successurl='${pageContext.request.contextPath}/frontend.jsp';
<%
  }
%>		
</script>

<%
  if(request.getSession(false) != null) {
    Object sid = session.getAttribute("cas-sid");
    if(sid!=null && SessionManager.get().isOpen(sid.toString())) {
        response.sendRedirect(request.getContextPath()+"/frontend.jsp");
        return;
    }
  }
%>


<%@ include file="body.jsp" %>

<%@ include file="footer.jsp" %>

<link REL="STYLESHEET" HREF="<%=MODULE%>/sc/skins/<%=SKIN%>/style-login.css" TYPE="text/css" />