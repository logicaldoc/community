<%@ page language="java" contentType="application/json" pageEncoding="UTF-8"%>
<%@ page import="com.logicaldoc.core.SystemInfo" %>
<%@ include file="android/setcookie.jsp" %>
{"installed":true,"maintenance":false,"needsDbUpgrade":false,"version":"<%=SystemInfo.get().getRelease()%>","versionstring":"<%=SystemInfo.get().getRelease()%>.0","productname":"LogicalDOC","product":"LogicalDOC"}
