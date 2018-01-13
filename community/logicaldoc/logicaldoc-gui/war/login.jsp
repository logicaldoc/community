<%@ page language="java" contentType="text/html; charset=UTF-8"
	pageEncoding="UTF-8"%>
<%! static String MODULE="login"; %>
<%! static String LOGIN_PAGE="login.jsp"; %>
<%@ include file="header.jsp" %>
<%@ include file="detectmobile.jsp" %>
<link REL="STYLESHEET" HREF="<%=MODULE%>/sc/skins/<%=SKIN%>/style-login.css" TYPE="text/css" />

<script type="text/javascript">
	var j_loginurl='${pageContext.request.contextPath}/<%=LOGIN_PAGE%>';
	var j_successurl='${pageContext.request.contextPath}/frontend.jsp';
	var j_failureurl=j_loginurl;
	var j_layout='desktop';
</script>


<%@ include file="body.jsp" %>
<%@ include file="footer.jsp" %>