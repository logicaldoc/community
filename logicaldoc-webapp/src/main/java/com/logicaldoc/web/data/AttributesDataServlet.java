package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.AttributeSetDAO;
import com.logicaldoc.core.metadata.Template;
import com.logicaldoc.core.metadata.TemplateDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for attribute sets data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5.4
 */
public class AttributesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, int max,
			Locale locale) throws PersistenceException, IOException {

		Long templateId = null;
		if (StringUtils.isNotEmpty(request.getParameter("templateId")))
			templateId = Long.parseLong(request.getParameter("templateId"));

		boolean docevent = "docevent".equals(request.getParameter("context"));

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		/*
		 * Put the standard attributes
		 */
		if (templateId == null) {
			if (docevent) {
				writer.print("<attribute>");
				writer.print("<name>date</name>");
				writer.print("<label><![CDATA[" + I18N.message("date", locale) + "]]></label>");
				writer.print("<type>" + Attribute.TYPE_DATE + "</type>");
				writer.print("</attribute>");

				writer.print("<attribute>");
				writer.print("<name>event</name>");
				writer.print("<label><![CDATA[" + I18N.message("event", locale) + "]]></label>");
				writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
				writer.print("</attribute>");

				writer.print("<attribute>");
				writer.print("<name>path</name>");
				writer.print("<label><![CDATA[" + I18N.message("path", locale) + "]]></label>");
				writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
				writer.print("</attribute>");

				writer.print("<attribute>");
				writer.print("<name>reason</name>");
				writer.print("<label><![CDATA[" + I18N.message("reason", locale) + "]]></label>");
				writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
				writer.print("</attribute>");

				writer.print("<attribute>");
				writer.print("<name>user</name>");
				writer.print("<label><![CDATA[" + I18N.message("user", locale) + "]]></label>");
				writer.print("<type>" + Attribute.TYPE_USER + "</type>");
				writer.print("</attribute>");
			}

			writer.print("<attribute>");
			writer.print("<name>filename</name>");
			writer.print("<label><![CDATA[" + I18N.message("filename", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>customId</name>");
			writer.print("<label><![CDATA[" + I18N.message("customid", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>lastModified</name>");
			writer.print("<label><![CDATA[" + I18N.message("lastmodified", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_DATE + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>created</name>");
			writer.print("<label><![CDATA[" + I18N.message("createdon", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_DATE + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>published</name>");
			writer.print("<label><![CDATA[" + I18N.message("publishedon", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_DATE + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>fileVersion</name>");
			writer.print("<label><![CDATA[" + I18N.message("fileversion", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>version</name>");
			writer.print("<label><![CDATA[" + I18N.message("version", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>type</name>");
			writer.print("<label><![CDATA[" + I18N.message("type", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>size</name>");
			writer.print("<label><![CDATA[" + I18N.message("size", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_INT + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>pages</name>");
			writer.print("<label><![CDATA[" + I18N.message("pages", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_INT + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>creator</name>");
			writer.print("<label><![CDATA[" + I18N.message("creator", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>publisher</name>");
			writer.print("<label><![CDATA[" + I18N.message("publisher", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>comment</name>");
			writer.print("<label><![CDATA[" + I18N.message("comment", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>template</name>");
			writer.print("<label><![CDATA[" + I18N.message("template", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>tags</name>");
			writer.print("<label><![CDATA[" + I18N.message("tags", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>workflowStatus</name>");
			writer.print("<label><![CDATA[" + I18N.message("workflowstatus", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>startPublishing</name>");
			writer.print("<label><![CDATA[" + I18N.message("startpublishing", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_DATE + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>stopPublishing</name>");
			writer.print("<label><![CDATA[" + I18N.message("stoppublishing", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_DATE + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>folder</name>");
			writer.print("<label><![CDATA[" + I18N.message("folder", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>folderId</name>");
			writer.print("<label><![CDATA[" + I18N.message("folderId", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_INT + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>score</name>");
			writer.print("<label><![CDATA[" + I18N.message("score", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_INT + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>rating</name>");
			writer.print("<label><![CDATA[" + I18N.message("rating", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_INT + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>language</name>");
			writer.print("<label><![CDATA[" + I18N.message("language", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_STRING + "</type>");
			writer.print("</attribute>");

			writer.print("<attribute>");
			writer.print("<name>tenantId</name>");
			writer.print("<label><![CDATA[" + I18N.message("tenantId", locale) + "]]></label>");
			writer.print("<type>" + Attribute.TYPE_INT + "</type>");
			writer.print("</attribute>");
		}

		/*
		 * Iterate over the collection of extended attributes
		 */
		Map<String, Attribute> attributes = new HashMap<String, Attribute>();
		if (templateId == null) {
			AttributeSetDAO dao = (AttributeSetDAO) Context.get().getBean(AttributeSetDAO.class);
			attributes = dao.findAttributes(session.getTenantId(), null);
		} else {
			TemplateDAO dao = (TemplateDAO) Context.get().getBean(TemplateDAO.class);
			Template template = dao.findById(templateId);
			dao.initialize(template);
			List<String> names = template.getAttributeNames();
			for (String name : names)
				attributes.put(name, template.getAttribute(name));
		}

		for (String name : attributes.keySet()) {
			Attribute attribute = attributes.get(name);
			if (attribute.getHidden() == 1)
				continue;

			writer.print("<attribute>");
			writer.print("<name><![CDATA[ext_" + name + "]]></name>");
			writer.print("<label><![CDATA[" + (StringUtils.isNotEmpty(attribute.getLabel()) ? attribute.getLabel() : "")
					+ "]]></label>");
			writer.print("<type>" + attribute.getType() + "</type>");
			writer.print("</attribute>");
		}

		writer.write("</list>");
	}
}