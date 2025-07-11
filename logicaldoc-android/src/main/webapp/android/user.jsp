<%@ page language="java" contentType="application/json" pageEncoding="UTF-8"%>
<%@ page import="java.util.Base64" %>
<%@ page import="com.logicaldoc.core.security.user.UserDAO" %>
<%@ page import="com.logicaldoc.core.security.user.User" %>
<%@ page import="com.logicaldoc.util.spring.Context" %>
<%
   String username = "admin";
   String displayName = "admin";
   String email = "";
   
   try{
     String authorization = request.getHeader("authorization");
     if (authorization != null){
        authorization = authorization.substring(authorization.indexOf(" ")).trim();
        byte[] decodedBytes = Base64.getDecoder().decode(authorization);
        String decodedString = new String(decodedBytes);
   
        username = decodedString.substring(0, decodedString.indexOf(":"));
        
        UserDAO dao = (UserDAO) Context.get().getBean(UserDAO.class);
        User user = dao.findByUsername(username);
        displayName = user.getFullName();
        email = user.getEmail();
     }
   }catch(Throwable t){
   }
%>
<%@ include file="setcookie.jsp" %>
{"lds":{"meta":{"status":"ok","statuscode":200,"message":"OK","totalitems":"","itemsperpage":""},"data":{"id":"<%=username%>","display-name":"<%=displayName%>","email":"<%=email%>","language":"en"}}}
