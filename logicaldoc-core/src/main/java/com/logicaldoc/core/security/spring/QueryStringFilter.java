package com.logicaldoc.core.security.spring;

import java.io.IOException;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.GenericFilterBean;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * Since there is no way to replicate the original query string in
 * &lt;form-login login-page="..."&gt;, we define this custom filter that does
 * the job(we suggest to put it after REQUEST_CACHE_FILTER).
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class QueryStringFilter extends GenericFilterBean {

    private static final Logger log = LoggerFactory.getLogger(QueryStringFilter.class);

    /**
     * The login page to redirect to
     */
    private String loginPage = "/login.jsp";

    /**
     * List of base URIs to consider, all the rest will be skipped
     */
    private List<String> baseUris = List.of("/display", "/frontend", "/mobile", "/download");

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain)
            throws IOException, ServletException {

        HttpServletRequest request = (HttpServletRequest) req;
        HttpServletResponse response = (HttpServletResponse) res;
        String queryString = request.getQueryString();

        /*
         * If we are NOT on the login page and there is no authentication
         * context, then we redirect to login page replicating the query string
         * of the original request
         */
        if (baseUris.stream().anyMatch(baseUri -> request.getRequestURI().startsWith(baseUri))
                && (SecurityContextHolder.getContext().getAuthentication() == null
                        || !SecurityContextHolder.getContext().getAuthentication().isAuthenticated())
                && StringUtils.isNotEmpty(queryString)) {

            String redirectUrl = "%s?%s".formatted(loginPage, queryString);

            log.debug("Redirecting to {}", redirectUrl);
            response.sendRedirect(redirectUrl);
            return;
        }

        chain.doFilter(req, res);
    }

    public String getLoginPage() {
        return loginPage;
    }

    public void setLoginPage(String loginPage) {
        this.loginPage = loginPage;
    }
}
