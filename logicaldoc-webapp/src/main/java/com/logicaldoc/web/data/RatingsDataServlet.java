package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.Locale;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Rating;
import com.logicaldoc.core.document.RatingDAO;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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

        PrintWriter writer = response.getWriter();

        /*
         * Iterate over records composing the response XML document
         */
        writer.print("<list>");
        for (Rating rating : RatingDAO.get().findByDocId(Long.parseLong(docId))) {
            writer.print("<rating>");
            writer.print(String.format("<id>%d</id>", rating.getId()));
            writer.print(String.format("<user><![CDATA[%s]]></user>", rating.getUsername()));
            writer.print(String.format("<vote>%d</vote>", rating.getVote()));
            writer.print(String.format("<date>%s</date>", df.format(rating.getLastModified())));
            writer.print("</rating>");
        }
        writer.print("</list>");
    }
}