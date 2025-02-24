<%@ page language="java" pageEncoding="UTF-8"%>
<%@ page import="com.logicaldoc.core.security.SessionManager"%>
<%!
   public void setCookie(HttpServletRequest request, HttpServletResponse response) {
		try {
		  String sid = SessionManager.get().getSessionId(request);
		  if (sid != null) {
			  Cookie sidCookie = new Cookie("ldoc-sid", sid);
			  sidCookie.setMaxAge(1800); 
			  sidCookie.setHttpOnly(true);
			  response.addCookie(sidCookie);
		  }
		} catch (Throwable t) {} 
   }
%>
<% setCookie(request, response); %>