package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collection;
import java.util.List;
import java.util.Locale;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.conversion.FormatConversionManager;
import com.logicaldoc.core.conversion.FormatConverter;
import com.logicaldoc.core.conversion.NotAvailableConverter;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.menu.MenuDAO;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is responsible for conversion formats data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class FormatConvertersDataServlet extends AbstractDataServlet {

	private static final String ID = "<id><![CDATA[%s]]></id>";

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {

		String in = request.getParameter("in");
		String out = request.getParameter("out");
		String converterSpecification = request.getParameter("converter");

		MenuDAO mDao = MenuDAO.get();
		boolean parameters = session.getTenantId() == Tenant.DEFAULT_ID
				&& mDao.isReadAllowed(1750, session.getUserId());

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
		FormatConversionManager manager = FormatConversionManager.get();

		// Get all the possible associations of a specific converter
		for (String inExt : manager.getAvailableInputFormats()) {
			for (String outExt : manager.getAllOutputFormats(inExt)) {
				String id = "%s-%s".formatted(inExt, outExt);
				List<FormatConverter> converters = manager.getConverters().get(id);
				for (FormatConverter formatConverter : converters) {
					if (converterSpecification.equals(formatConverter.getClass().getName())) {
						FormatConverter associatedConverter = manager.getConverter(inExt, outExt);
						writer.print("<association>");
						writer.print(String.format(ID, id));
						writer.print(String.format("<in><![CDATA[%s]]></in>", inExt));
						writer.print(String.format("<out><![CDATA[%s]]></out>", outExt));
						writer.print(String.format("<converter><![CDATA[%s]]></converter>",
								associatedConverter.getClass().getName()));
						writer.print(String.format("<selected>%s</selected>",
								converterSpecification.equals(associatedConverter.getClass().getName())));
						writer.print(String.format("<eenabled>%b</eenabled>", associatedConverter.isEnabled()));
						writer.print("</association>");
					}
				}
			}
		}
	}

	private void writeConverters(PrintWriter writer, String in, String out, boolean parameters) {

		Collection<FormatConverter> converters;
		if (in.equals("-") && out.equals("-")) {
			// Get all configured converters
			converters = FormatConversionManager.get().getAllConverters();
		} else {
			// Get possible converters for a specific couple of formats
			converters = FormatConversionManager.get().getAvailableConverters(in, out);
		}

		for (FormatConverter converter : converters) {
			writeConverter(writer, in, out, converter, parameters);
		}
	}

	private void writeConverters(PrintWriter writer, boolean parameters) {
		FormatConversionManager manager = FormatConversionManager.get();
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
		ContextProperties conf = Context.get().getConfig();

		if (converter == null || converter.getClass().equals(NotAvailableConverter.class))
			return;

		writer.print("<converter>");
		if ("-".equals(inExt) && "-".equals(outExt))
			writer.print(String.format(ID, converter.getClass().getName()));
		else
			writer.print(String.format("<id><![CDATA[%s-%s]]></id>", inExt, outExt));
		writer.print(String.format("<in><![CDATA[%s]]></in>", inExt));
		writer.print(String.format("<out><![CDATA[%s]]></out>", outExt));
		writer.print(String.format("<converter><![CDATA[%s]]></converter>", converter.getClass().getName()));
		writer.print(String.format("<label><![CDATA[%s]]></label>", converter.getClass().getSimpleName()));
		writer.print(String.format("<eenabled>%b</eenabled>", converter.isEnabled()));

		if (parameters) {
			for (String name : converter.getParameterNames()) {
				String value = conf.getProperty("converter.%s.%s".formatted(converter.getClass().getSimpleName(), name),
						"");
				writer.print(String.format("<%s><![CDATA[%s]]></%s>", name, value, name));
			}
		}

		writer.print("</converter>");
	}
}
