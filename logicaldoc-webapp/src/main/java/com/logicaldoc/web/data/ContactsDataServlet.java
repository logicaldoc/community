package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.contact.Contact;
import com.logicaldoc.core.contact.ContactDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for contacts data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class ContactsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, int max,
			Locale locale)
			throws PersistenceException, IOException {
		
		long userId = Long.parseLong(request.getParameter("userId"));

		if (userId != session.getUserId())
			throw new IOException("Permission denied");

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
	}
}
