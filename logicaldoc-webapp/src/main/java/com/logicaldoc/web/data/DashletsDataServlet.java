package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.dao.BookmarkDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.i18n.I18N;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.LocaleUtil;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for dashlets data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class DashletsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DashletsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			Session session = ServiceUtil.validateSession(request);

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			Locale locale = request.getParameter("locale") != null ? LocaleUtil.toLocale(request.getParameter("locale"))
					: Locale.ENGLISH;

			/*
			 * Iterate over the collection of bookmarks
			 */
			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			List<Object> records = new ArrayList<Object>();
			BookmarkDAO dao = (BookmarkDAO) Context.get().getBean(BookmarkDAO.class);

			/*
			 * Search for folders first.
			 */
			StringBuffer query = new StringBuffer("select A.id, A.name, A.title, A.type, A.query, A.content"
					+ " from Dashlet A where A.deleted = 0 and A.tenantId = " + session.getTenantId()
					+ " order by A.id asc");
			records.addAll(dao.findByQuery(query.toString(), null, null));

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Object record : records) {
				Object[] cols = (Object[]) record;

				writer.print("<dashlet>");
				writer.print("<id>" + cols[0] + "</id>");
				writer.print("<name><![CDATA[" + (cols[1] == null ? "" : cols[1]) + "]]></name>");
				writer.print("<title><![CDATA[" + (cols[2] == null ? "" : I18N.message(cols[2].toString(), locale))
						+ "]]></title>");
				writer.print("<type>" + (cols[3] == null ? "" : cols[3]) + "</type>");
				writer.print("<query><![CDATA[" + (cols[4] == null ? "" : cols[4]) + "]]></query>");
				writer.print("<content><![CDATA[" + (cols[5] == null ? "" : cols[5]) + "]]></content>");
				writer.print("</dashlet>");
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