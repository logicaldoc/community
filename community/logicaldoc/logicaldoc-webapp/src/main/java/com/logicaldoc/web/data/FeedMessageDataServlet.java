package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.rss.FeedMessage;
import com.logicaldoc.core.rss.dao.FeedMessageDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for feed message data.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class FeedMessageDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(FeedMessageDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			ServiceUtil.validateSession(request);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			/*
			 * Execute the Query
			 */
			Context context = Context.get();
			FeedMessageDAO dao = (FeedMessageDAO) context.getBean(FeedMessageDAO.class);

			List<FeedMessage> records = dao.findAll();

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			/*
			 * Iterate over records composing the response XML document
			 */
			for (FeedMessage record : records) {
				writer.print("<feedmessage>");
				writer.print("<id>" + record.getId() + "</id>");
				writer.print("<guid><![CDATA[" + record.getGuid() + "]]></guid>");
				writer.print("<title><![CDATA[" + record.getTitle() + "]]></title>");
				writer.print("<description><![CDATA[" + record.getDescription() + "]]></description>");
				writer.print("<link><![CDATA[" + record.getLink() + "]]></link>");
				writer.print("<pubDate>" + df.format(record.getPubDate()) + "</pubDate>");
				writer.print("<read>" + record.getRead() + "</read>");
				writer.print("</feedmessage>");
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
