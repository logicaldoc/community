package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.contact.Contact;
import com.logicaldoc.core.contact.ContactDAO;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Contact contact : ContactDAO.get().findByUser(userId, null)) {
			if (contact.getDeleted() == 1)
				continue;

			writer.print("<contact>");
			writer.print(String.format("<id>%d</id>", contact.getId()));
			writer.print(String.format("<email><![CDATA[%s]]></email>", contact.getEmail()));
			writer.print(String.format("<firstName><![CDATA[%s]]></firstName>",
					StringUtils.defaultString(contact.getFirstName())));
			writer.print(String.format("<lastName><![CDATA[%s]]></lastName>",
					StringUtils.defaultString(contact.getLastName())));
			writer.print(String.format("<company><![CDATA[%s]]></company>",
					StringUtils.defaultString(contact.getCompany())));
			writer.print(
					String.format("<mobile><![CDATA[%s]]></mobile>", StringUtils.defaultString(contact.getMobile())));
			writer.print(String.format("<phone><![CDATA[%s]]></phone>", StringUtils.defaultString(contact.getPhone())));
			writer.print(String.format("<address><![CDATA[%s]]></address>",
					StringUtils.defaultString(contact.getAddress())));
			writer.print("</contact>");
		}

		writer.write("</list>");
	}
}
