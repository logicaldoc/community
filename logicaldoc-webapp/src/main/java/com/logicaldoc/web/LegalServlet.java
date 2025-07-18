package com.logicaldoc.web;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Base64;
import java.util.Base64.Decoder;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.util.spring.Context;
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

	private static final Logger log = LoggerFactory.getLogger(AvatarServlet.class);
	
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
			String content = Context.get(DocumentDAO.class)
					.queryForString("select ld_content from ld_legal where ld_name = :legal", Map.of("legal", legal));
			ServletUtil.setContentDisposition(request, response, legal + ".pdf");
			Decoder decoder = Base64.getDecoder();
			byte[] lefgalBytes = decoder.decode(content);
			try (ByteArrayInputStream bis = new ByteArrayInputStream(lefgalBytes)) {
				IOUtils.write(lefgalBytes, response.getOutputStream());
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			ServletUtil.sendError(response, e.getMessage());
		}
	}
}