<%if(request.getQueryString()!=null && !"".equals(request.getQueryString())){
    response.sendRedirect("login.jsp?"+request.getQueryString());
}else{
    response.sendRedirect("login.jsp");
}%>