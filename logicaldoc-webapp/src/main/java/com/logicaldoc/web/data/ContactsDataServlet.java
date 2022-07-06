package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.contact.Contact;
import com.logicaldoc.core.contact.ContactDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for contacts data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class ContactsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(ContactsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);
			long userId = Long.parseLong(request.getParameter("userId"));

			if (userId != session.getUserId())
				throw new Exception("Permission denied");

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			ContactDAO dao = (ContactDAO) Context.get().getBean(ContactDAO.class);

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Contact contact : dao.findByUser(userId, null)) {
				if (contact.getDeleted() == 1)
					continue;

				writer.print("<contact>");
				writer.print("<id>" + contact.getId() + "</id>");
				writer.print("<email><![CDATA[" + contact.getEmail() + "]]></email>");
				if (contact.getFirstName() != null)
					writer.print("<firstName><![CDATA[" + contact.getFirstName() + "]]></firstName>");
				if (contact.getLastName() != null)
					writer.print("<lastName><![CDATA[" + contact.getLastName() + "]]></lastName>");
				if (contact.getCompany() != null)
					writer.print("<company><![CDATA[" + contact.getCompany() + "]]></company>");
				writer.print("</contact>");
			}

			writer.write("</list>");
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			if (e instanceof ServletException)
				throw (ServletException) e;
			else if (e instanceof IOException)
				throw (IOException) e;
			else
				throw new ServletException(e.getMessage(), e);
		}
	}
}
