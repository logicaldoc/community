<%@ page import="jakarta.servlet.*" %>
<%@ page import="jakarta.servlet.http.*" %>
<%@ page import="java.util.regex.*" %>
<%@ page import="java.util.*" %>
<%@ page import="com.logicaldoc.core.ticket.*" %>
<%@ page import="com.logicaldoc.core.security.*" %>
<%@ page import="com.logicaldoc.util.*" %>
<%@ page import="com.logicaldoc.util.spring.*" %>
<%@ page import="com.logicaldoc.web.util.*" %>
<%@ page import="com.logicaldoc.i18n.*" %>
<%
  Long docId = null;
  String ticketId = request.getParameter("ticketId");
    
  com.logicaldoc.core.document.DocumentDAO docDao = (com.logicaldoc.core.document.DocumentDAO)Context.get().getBean(com.logicaldoc.core.document.DocumentDAO.class);
  
  Long userId = null;
  Ticket ticket = null;
  if(ticketId != null) {
    com.logicaldoc.core.document.DocumentDAO docDao = Context.get(com.logicaldoc.core.document.DocumentDAO.class);
    ticket = tDao.findByTicketId(ticketId);
  	if(ticket != null && ticket.isTicketViewExpired())
  	   ticket = null;
    else
       docId = ticket.getDocId();
  } else {
    docId = Long.parseLong(request.getParameter("docId"));
    com.logicaldoc.core.security.Session ldSession = ServletUtil.validateSession(request);
    userId = ldSession.getUserId();
  }  
 
  String locale = "en";
  if(request.getParameter("locale")!=null && !request.getParameter("locale").isEmpty())
      locale = request.getParameter("locale");
 
  boolean read = ticket == null && docDao.isReadAllowed(docId, userId);
  boolean preview = ticket == null && docDao.isPreviewAllowed(docId, userId);
 
  if(ticket == null && (!read || !preview)) {
      if(!read)
         response.getWriter().write(I18N.message("youdonothavereadpermissionondoc", locale));
      else
         response.getWriter().write(I18N.message("youdonothavepreviewpermissionondoc", locale));
      return;
  }
  
  boolean print = ticket !=null ? !ticket.isTicketExpired() : docDao.isPrintAllowed(docId, userId);
  boolean download = ticket !=null ? !ticket.isTicketExpired() : docDao.isDownloadAllowed(docId, userId);
  
  /*
   * Prepares an hash so the servlet could check that this request comes from a real preview panel.
   * Also store a session attribute containing the docId as additional information used by the servlet
   * to prevent a user without download permission to download the file being previewed.
   */ 
  com.logicaldoc.core.document.Document doc = docDao.findById(docId);
  Integer previewCheck=null;
  if(doc!=null) {
	 if(doc.getDigest()!=null)
    	previewCheck=(doc.getDigest() + doc.getFileName()).hashCode();
	 else
	    previewCheck=doc.getFileName().hashCode();
	 
	 session.setAttribute("preview-"+docId, new Date());   
  }
  
  String path = "convertpdf";
  if(request.getParameter("path")!=null)
  	path = request.getParameter("path");
  if(ticket!=null)
  	path = "download-ticket";
  
  String query = request.getQueryString();
  if(previewCheck!=null)
    query += "&previewcheck="+previewCheck;
  if(ticket != null) {
  	query += "&suffix=conversion.pdf";
    session.setAttribute("preview-"+ticketId, new Date());   
  }
%>
<%@ include file="preview.jsp" %>