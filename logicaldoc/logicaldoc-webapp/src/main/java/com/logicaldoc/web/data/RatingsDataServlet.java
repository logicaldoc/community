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

import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.document.dao.RatingDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet is responsible for ratings data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class RatingsDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(RatingsDataServlet.class);

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response) throws ServletException,
			IOException {
		try {
			ServiceUtil.validateSession(request);

			String docId = request.getParameter("docId");

			response.setContentType("text/xml");
			response.setCharacterEncoding("UTF-8");

			// Avoid resource caching
			response.setHeader("Pragma", "no-cache");
			response.setHeader("Cache-Control", "no-store");
			response.setDateHeader("Expires", 0);

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			RatingDAO ratingDao = (RatingDAO) Context.get().getBean(RatingDAO.class);
			List<Rating> ratings = ratingDao.findByDocId(Long.parseLong(docId));

			PrintWriter writer = response.getWriter();
			writer.print("<list>");

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Rating rating : ratings) {
				writer.print("<rating>");
				writer.print("<id>" + rating.getId() + "</id>");
				writer.print("<user><![CDATA[" + rating.getUsername() + "]]></user>");
				writer.print("<vote>" + rating.getVote() + "</vote>");
				writer.print("<date>" + df.format(rating.getLastModified()) + "</date>");
				writer.print("</rating>");
			}

			writer.print("</list>");

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