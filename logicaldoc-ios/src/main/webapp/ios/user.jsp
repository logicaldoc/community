<%@ page language="java" contentType="application/json" pageEncoding="UTF-8"%>
<%@ page import="javax.servlet.*" %>
<%@ page import="javax.servlet.http.*" %>
<%@ page import="java.util.*" %>
<%@ page import="java.io.*" %>
<%@ page import="com.logicaldoc.core.security.user.*" %>
<%@ page import="com.logicaldoc.util.*" %>

<%
   String username="admin";
   String displayName="admin";
   String email="";
   
   try{
     String authorization = request.getHeader("authorization");
     if(authorization!=null){
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
{"lds":{"meta":{"status":"ok","statuscode":100,"message":"OK","totalitems":"","itemsperpage":""},"data":{"id":"<%=username%>","display-name":"<%=displayName%>","email":"<%=email%>"}}}
