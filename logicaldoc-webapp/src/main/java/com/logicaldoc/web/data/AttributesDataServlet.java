package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.i18n.I18N;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for attribute sets data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.4
 */
public class AttributesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		Long templateId = null;
		if (StringUtils.isNotEmpty(request.getParameter("templateId")))
			templateId = Long.parseLong(request.getParameter("templateId"));

		boolean docevent = "docevent".equals(request.getParameter("context"));

		boolean sections = "true".equals(request.getParameter("sections"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		/*
		 * Put the standard attributes
		 */
		if (templateId == null) {
			if (docevent) {
				printAttibute("date", I18N.message("date", locale), Attribute.TYPE_DATE, writer);
				printAttibute("event", I18N.message("event", locale), writer);
				printAttibute("path", I18N.message("path", locale), writer);
				printAttibute("reason", I18N.message("reason", locale), writer);
				printAttibute("user", I18N.message("user", locale), Attribute.TYPE_USER, writer);
			}

			printAttibute("customId", I18N.message("customid", locale), writer);
			printAttibute("filename", I18N.message("filename", locale), writer);
			printAttibute("lastModified", I18N.message("lastmodified", locale), writer);
			printAttibute("user", I18N.message("user", locale), Attribute.TYPE_DATE, writer);
			printAttibute("created", I18N.message("createdon", locale), Attribute.TYPE_DATE, writer);
			printAttibute("published", I18N.message("publishedon", locale), Attribute.TYPE_DATE, writer);
			printAttibute("fileVersion", I18N.message("fileversion", locale), writer);
			printAttibute("version", I18N.message("version", locale), writer);
			printAttibute("revision", I18N.message("revision", locale), writer);
			printAttibute("type", I18N.message("type", locale), writer);
			printAttibute("size", I18N.message("size", locale), Attribute.TYPE_INT, writer);
			printAttibute("pages", I18N.message("pages", locale), Attribute.TYPE_INT, writer);
			printAttibute("creator", I18N.message("creator", locale), writer);
			printAttibute("publisher", I18N.message("publisher", locale), writer);
			printAttibute("comment", I18N.message("comment", locale), writer);
			printAttibute("lastNote", I18N.message("lastnote", locale), writer);
			printAttibute("template", I18N.message("template", locale), writer);
			printAttibute("tags", I18N.message("tags", locale), writer);
			printAttibute("workflowStatus", I18N.message("workflowstatus", locale), writer);
			printAttibute("startPublishing", I18N.message("startpublishing", locale), Attribute.TYPE_DATE, writer);
			printAttibute("stopPublishing", I18N.message("stoppublishing", locale), Attribute.TYPE_DATE, writer);
			printAttibute("folder", I18N.message("folder", locale), writer);
			printAttibute("folderId", I18N.message("folderId", locale), Attribute.TYPE_INT, writer);
			printAttibute("score", I18N.message("score", locale), Attribute.TYPE_INT, writer);
			printAttibute("rating", I18N.message("rating", locale), Attribute.TYPE_INT, writer);
			printAttibute("language", I18N.message("language", locale), writer);
			printAttibute("tenantId", I18N.message("tenantId", locale), Attribute.TYPE_INT, writer);
		}

		/*
		 * Iterate over the collection of extended attributes
		 */
		Map<String, Attribute> attributes = new HashMap<>();
		if (templateId == null) {
			AttributeSetDAO dao = AttributeSetDAO.get();
			attributes = dao.findAttributes(session.getTenantId(), null);
		} else {
			TemplateDAO dao = TemplateDAO.get();
			Template template = dao.findById(templateId);
			dao.initialize(template);
			List<String> names = template.getAttributeNames();
			for (String name : names)
				attributes.put(name, template.getAttribute(name));
		}

		for (Map.Entry<String, Attribute> entry : attributes.entrySet()) {
			Attribute attribute = entry.getValue();
			if (attribute.isHidden() || (!sections && attribute.isSection()))
				continue;

			printAttibute("ext_" + entry.getKey(),
					(StringUtils.isNotEmpty(attribute.getLabel()) ? attribute.getLabel() : ""), attribute.getType(),
					writer);
		}

		writer.write("</list>");
	}

	private void printAttibute(String name, String label, PrintWriter writer) {
		printAttibute(name, label, Attribute.TYPE_STRING, writer);
	}

	private void printAttibute(String name, String label, int type, PrintWriter writer) {
		writer.print("<attribute>");
		writer.print("<name>" + name + "</name>");
		writer.print("<label><![CDATA[" + label + "]]></label>");
		writer.print("<type>" + type + "</type>");
		writer.print("</attribute>");
	}
}