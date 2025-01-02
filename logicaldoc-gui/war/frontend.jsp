<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%! static String MODULE="frontend"; %>
<%! static String LOGIN_PAGE="login.jsp"; %>
<%@ include file="header.jsp" %>

<script src="<%=MODULE%>/ace/ace.js"></script>
<script src="<%=MODULE%>/ace/ext-language_tools.js"></script>
<script src="<%=MODULE%>/ace/snippets/velocity.js" type="text/javascript" charset="utf-8"></script>
<script src="<%=MODULE%>/ace/mode-velocity.js" type="text/javascript" charset="utf-8"></script>
<script src="<%=MODULE%>/ace/theme-eclipse.js" type="text/javascript" charset="utf-8"></script>

<%@ include file="detectmobile.jsp" %>

<script type="text/javascript">
	var j_loginurl='${pageContext.request.contextPath}/<%=LOGIN_PAGE%>';
	var j_successurl='${pageContext.request.contextPath}/frontend.jsp';
	var j_failureurl=j_loginurl;
</script>

<%@ include file="body.jsp" %>

<!-- Make sure custom stype is last loaded -->
<link REL="STYLESHEET" HREF="<%=MODULE%>/sc/skins/<%=SKIN%>/style.css" TYPE="text/css" />

<%@ include file="footer.jsp" %>
