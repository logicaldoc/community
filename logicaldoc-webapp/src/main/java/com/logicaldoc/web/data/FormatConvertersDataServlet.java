package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConverter;
import com.logicaldoc.core.conversion.FormatConverterManager;
import com.logicaldoc.core.conversion.NotAvailableConverter;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * This servlet is responsible for conversion formats data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class FormatConvertersDataServlet extends AbstractDataServlet {

	private static final String CLOSE_ID = "]]></id>";

	private static final String ID = "<id><![CDATA[";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		String in = request.getParameter("in");
		String out = request.getParameter("out");
		String converterSpecification = request.getParameter("converter");

		MenuDAO mDao = Context.get(MenuDAO.class);
		boolean parameters = session.getTenantId() == Tenant.DEFAULT_ID && mDao.isReadEnable(1750, session.getUserId());

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		if (!StringUtils.isEmpty(converterSpecification)) {
			printConverters(writer, converterSpecification);
		} else if (!StringUtils.isEmpty(in) && !StringUtils.isEmpty(out)) {
			writeConverters(writer, in, out, parameters);
		} else {
			writeConverters(writer, parameters);
		}
		writer.write("</list>");
	}

	private void printConverters(PrintWriter writer, String converterSpecification) {
		FormatConverterManager manager = getManager();

		// Get all the possible associations of a specific converter
		for (String inExt : manager.getAvailableInputFormats()) {
			for (String outExt : manager.getAllOutputFormats(inExt)) {
				String id = inExt + "-" + outExt;
				List<FormatConverter> converters = manager.getConverters().get(id);
				for (FormatConverter formatConverter : converters) {
					if (converterSpecification.equals(formatConverter.getClass().getName())) {
						FormatConverter associatedConverter = manager.getConverter(inExt, outExt);
						writer.print("<association>");
						writer.print(ID + id + CLOSE_ID);
						writer.print("<in><![CDATA[" + inExt + "]]></in>");
						writer.print("<out><![CDATA[" + outExt + "]]></out>");
						writer.print(
								"<converter><![CDATA[" + associatedConverter.getClass().getName() + "]]></converter>");
						writer.print(
								"<selected>" + converterSpecification.equals(associatedConverter.getClass().getName())
										+ "</selected>");
						writer.print("<eenabled>" + associatedConverter.isEnabled() + "</eenabled>");
						writer.print("</association>");
					}
				}
			}
		}
	}

	private void writeConverters(PrintWriter writer, String in, String out, boolean parameters) {
		FormatConverterManager manager = getManager();
		Collection<FormatConverter> converters;
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
	}

	private FormatConverterManager getManager() {
		FormatConverterManager manager = Context.get(FormatConverterManager.class);
		manager.getConverters();
		return manager;
	}

	private void writeConverters(PrintWriter writer, boolean parameters) {
		FormatConverterManager manager = getManager();
		// Get the full list of associations
		for (String inExt : manager.getAvailableInputFormats()) {
			for (String outExt : manager.getAllOutputFormats(inExt)) {
				FormatConverter converter = manager.getConverter(inExt, outExt);
				writeConverter(writer, inExt, outExt, converter, parameters);
			}
		}
	}

	private void writeConverter(PrintWriter writer, String inExt, String outExt, FormatConverter converter,
			boolean parameters) {
		ContextProperties conf = Context.get().getProperties();

		if (converter.getClass().equals(NotAvailableConverter.class))
			return;

		writer.print("<converter>");
		if ("-".equals(inExt) && "-".equals(outExt))
			writer.print(ID + converter.getClass().getName() + CLOSE_ID);
		else
			writer.print(ID + inExt + "-" + outExt + CLOSE_ID);
		writer.print("<in><![CDATA[" + inExt + "]]></in>");
		writer.print("<out><![CDATA[" + outExt + "]]></out>");
		writer.print("<converter><![CDATA[" + converter.getClass().getName() + "]]></converter>");
		writer.print("<label><![CDATA[" + converter.getClass().getSimpleName() + "]]></label>");
		writer.print("<eenabled>" + converter.isEnabled() + "</eenabled>");

		if (parameters) {
			for (String name : converter.getParameterNames()) {
				String value = conf.getProperty(
						"converter." + converter.getClass().getSimpleName() + "." + name, "");
				writer.print("<" + name + "><![CDATA[" + value + "]]></" + name + ">");
			}
		}

		writer.print("</converter>");
	}
}
