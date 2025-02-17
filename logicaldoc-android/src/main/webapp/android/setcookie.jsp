<%@ page language="java" pageEncoding="UTF-8"%>
<%@ page import="com.logicaldoc.core.security.SessionManager"%>
<%!
   public void setCookie(HttpServletRequest request, HttpServletResponse response) {
		try {
		  String sid = SessionManager.get().getSessionId(request);
		  if (sid != null) {
			  Cookie sidCookie = new Cookie("ldoc-sid", sid);
			  response.addCookie(sidCookie);
		  }
		} catch (Throwable t) {
			t.printStackTrace();
		} 
   }
%>
<% setCookie(request, response); %>