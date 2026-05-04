package com.logicaldoc.web;

import java.io.IOException;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * This servlet is used to serve those requests for displaying details of a
 * document or a folder. Basically it redirect to /frontend.jsp replicating the
 * received query string. It the query string is empty, then the servlet path is
 * analyzed: the firstsegment is interpreted as the parameter's name and the
 * second one as parameter's value
 * 
 * @author Marco Meschieri - LogicaLDOC
 * @since 7.5.1
 */
public class DisplayServlet extends HttpServlet {

    private static final long serialVersionUID = 1L;

    private static final Logger log = LoggerFactory.getLogger(DisplayServlet.class);

    public DisplayServlet() {
        super();
    }

    /**
     * Redirects the request to the proper frontend URL
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) {
        String query = StringUtils.defaultString(request.getQueryString());

        try {
            if (StringUtils.isNoneEmpty(query)) {
                response.sendRedirect("%s/frontend.jsp?%s".formatted(request.getContextPath(), query));
            } else {
                String[] pathInfo = request.getPathInfo().substring(1).split("/");
                response.sendRedirect(
                        "%s/frontend.jsp?%s=%s".formatted(request.getContextPath(), pathInfo[0], pathInfo[1]));
            }
        } catch (IOException e) {
            log.error(e.getMessage(), e);
            try {
                response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage());
            } catch (IOException ioe) {
                // Nothing to do
            }
        }
    }
}