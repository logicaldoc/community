package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.conversion.FormatConverter;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.conversion.NotAvailableConverter;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.MenuDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for conversion formats data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class FormatConvertersDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(FormatConvertersDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			String in = request.getParameter("in");
			String out = request.getParameter("out");
			String converterSpecification = request.getParameter("converter");

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			MenuDAO mDao = (MenuDAO) Context.get().getBean(MenuDAO.class);
			boolean parameters = session.getTenantId() == Tenant.DEFAULT_ID
					&& mDao.isReadEnable(1750, session.getUserId());

			FormatConverterManager manager = (FormatConverterManager) Context.get().getBean(
					FormatConverterManager.class);
			manager.getConverters();
			
			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			if (!StringUtils.isEmpty(converterSpecification)) {
				// Get all the possible associations of a specific converter
				for (String inExt : manager.getAvailableInputFormats()) {
					for (String outExt : manager.getAllOutputFormats(inExt)) {
						String id = inExt + "-" + outExt;
						List<FormatConverter> converters = manager.getConverters().get(id);
						for (FormatConverter formatConverter : converters) {
							if (converterSpecification.equals(formatConverter.getClass().getName())) {
								FormatConverter associatedConverter = manager.getConverter(inExt, outExt);
								writer.print("<association>");
								writer.print("<id><![CDATA[" + id + "]]></id>");
								writer.print("<in><![CDATA[" + inExt + "]]></in>");
								writer.print("<out><![CDATA[" + outExt + "]]></out>");
								writer.print("<converter><![CDATA[" + associatedConverter.getClass().getName()
										+ "]]></converter>");
								writer.print("<selected>" + converterSpecification.equals(associatedConverter)
										+ "</selected>");
								writer.print("<eenabled>" + associatedConverter.isEnabled() + "</eenabled>");
								writer.print("</association>");
							}
						}
					}
				}
			} else if (!StringUtils.isEmpty(in) && !StringUtils.isEmpty(out)) {
				Collection<FormatConverter> converters = new ArrayList<FormatConverter>();
				if (in.equals("-") && out.equals("-")) {
					// Get all configured converters
					converters = manager.getAllConverters();
				} else {
					// Get possible converters for a specific couple of formats
					converters = manager.getAvailableConverters(in, out);
				}

				for (FormatConverter converter : converters) {
					writeConverter(writer, in, out, converter, parameters);
				}
			} else {
				// Get the full list of associations
				for (String inExt : manager.getAvailableInputFormats()) {
					for (String outExt : manager.getAllOutputFormats(inExt)) {
						FormatConverter converter = manager.getConverter(inExt, outExt);
						writeConverter(writer, inExt, outExt, converter, parameters);
					}
				}
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

	private void writeConverter(PrintWriter writer, String inExt, String outExt, FormatConverter converter,
			boolean parameters) {
		ContextProperties conf = Context.get().getProperties();

		if (converter.getClass().equals(NotAvailableConverter.class))
			return;

		writer.print("<converter>");
		if ("-".equals(inExt) && "-".equals(outExt))
			writer.print("<id><![CDATA[" + converter.getClass().getName() + "]]></id>");
		else
			writer.print("<id><![CDATA[" + inExt + "-" + outExt + "]]></id>");
		writer.print("<in><![CDATA[" + inExt + "]]></in>");
		writer.print("<out><![CDATA[" + outExt + "]]></out>");
		writer.print("<converter><![CDATA[" + converter.getClass().getName() + "]]></converter>");
		writer.print("<label><![CDATA[" + converter.getClass().getSimpleName() + "]]></label>");
		writer.print("<eenabled>" + converter.isEnabled() + "</eenabled>");

		if (parameters) {
			for (String name : converter.getParameterNames()) {
				String value = conf.getPropertyWithSubstitutions("converter." + converter.getClass().getSimpleName()
						+ "." + name, "");
				writer.print("<" + name + "><![CDATA[" + value + "]]></" + name + ">");
			}
		}

		writer.print("</converter>");
	}
}
