package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.contact.Contact;
import com.logicaldoc.core.contact.ContactDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.spring.Context;

/**
 * This servlet is responsible for contacts data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class ContactsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		long userId = Long.parseLong(request.getParameter("userId"));

		if (userId != session.getUserId())
			throw new IOException("Permission denied");

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		ContactDAO dao = Context.get(ContactDAO.class);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Contact contact : dao.findByUser(userId, null)) {
			if (contact.getDeleted() == 1)
				continue;

			writer.print("<contact>");
			writer.print("<id>" + contact.getId() + "</id>");
			writer.print("<email><![CDATA[" + contact.getEmail() + "]]></email>");
			writer.print(
					"<firstName><![CDATA[" + StringUtils.defaultString(contact.getFirstName()) + "]]></firstName>");
			writer.print("<lastName><![CDATA[" + StringUtils.defaultString(contact.getLastName()) + "]]></lastName>");
			writer.print("<company><![CDATA[" + StringUtils.defaultString(contact.getCompany()) + "]]></company>");
			writer.print("<mobile><![CDATA[" + StringUtils.defaultString(contact.getMobile()) + "]]></mobile>");
			writer.print("<phone><![CDATA[" + StringUtils.defaultString(contact.getPhone()) + "]]></phone>");
			writer.print("<address><![CDATA[" + StringUtils.defaultString(contact.getAddress()) + "]]></address>");
			writer.print("</contact>");
		}

		writer.write("</list>");
	}
}
