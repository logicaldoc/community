package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.List;
import java.util.Locale;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.document.RatingDAO;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.util.Context;

/**
 * This servlet is responsible for ratings data.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
public class RatingsDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws PersistenceException, IOException {
		
		String docId = request.getParameter("docId");

		DateFormat df = getDateFormat();

		RatingDAO ratingDao = Context.get(RatingDAO.class);
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
	}
}