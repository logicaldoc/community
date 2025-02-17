<%@ page language="java" contentType="application/json" pageEncoding="UTF-8"%>
<%@ page import="com.logicaldoc.core.SystemInfo" %>
{"installed":true,"maintenance":false,"version":"<%=SystemInfo.get().getRelease()%>.0","versionstring":"<%=SystemInfo.get().getRelease()%>","edition":""}
