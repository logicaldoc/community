package com.logicaldoc.web;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Base64;
import java.util.Base64.Decoder;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.ResultSetWalker;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.web.util.ServletUtil;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet provides the content of a legal document
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 */
public class LegalServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(LegalServlet.class);

	/**
	 * Constructor of the object.
	 */
	public LegalServlet() {
		super();
	}

	/**
	 * The doGet method of the servlet. <br>
	 * 
	 * This method is called when a form has its tag value method equals to get.
	 * 
	 * @param request the request send by the client to the server
	 * @param response the response send by the server to the client
	 * @throws ServletException if an error occurred
	 * @throws IOException if an error occurred
	 */
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		try {
			String legal = request.getParameter("legal");

			if (StringUtils.isNotEmpty(legal))
				downloadLegal(request, response, legal);
			else
				listConfirmedLegals(request, response);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			ServletUtil.sendError(response, e.getMessage());
		}
	}

	private void listConfirmedLegals(HttpServletRequest request, HttpServletResponse response)
			throws IOException, PersistenceException {
		response.setContentType("text/xml");
		response.setCharacterEncoding("UTF-8");

		// Avoid resource caching
		response.setHeader("Cache-Control", "no-cache,no-store,must-revalidate");
		response.setHeader("Expires", "0");
		response.setHeader("Pragma", "no-cache");

		String userName = request.getParameter("user");
		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
		UserDAO.get().queryForResultSet(
				"select ld_name, ld_category, ld_title, B.ld_date from ld_legal A, ld_legal_confirmation B where ld_legal=ld_name and ld_username = :userName order by ld_sort asc, B.ld_date desc",
				Map.of("userName", userName), null, new ResultSetWalker() {

					@Override
					public void walk(ResultSet rows) throws SQLException {

						while (rows.next()) {
							writer.print("<legal>");
							writer.print("<name><![CDATA[" + rows.getString(1) + "]]></name>");
							writer.print("<category><![CDATA[" + rows.getString(2) + "]]></category>");
							writer.print("<title><![CDATA[" + rows.getString(3) + "]]></title>");
							writer.print("<confirmed>" + df.format(rows.getTimestamp(4)) + "</confirmed>");
							writer.print("</legal>");
						}
					}
				});

		writer.write("</list>");
	}

	private void downloadLegal(HttpServletRequest request, HttpServletResponse response, String legal)
			throws PersistenceException, IOException {
		String content = DocumentDAO.get()
				.queryForString("select ld_content from ld_legal where ld_name = :legal", Map.of("legal", legal));
		ServletUtil.setContentDisposition(request, response, legal + ".pdf");
		Decoder decoder = Base64.getDecoder();
		byte[] legalBytes = decoder.decode(content);
		try (ByteArrayInputStream bis = new ByteArrayInputStream(legalBytes)) {
			bis.transferTo(response.getOutputStream());
		}
	}
}