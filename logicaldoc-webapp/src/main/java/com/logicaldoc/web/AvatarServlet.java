package com.logicaldoc.web;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Base64;
import java.util.Base64.Decoder;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.util.UserUtil;
import com.logicaldoc.util.MimeType;
import com.logicaldoc.web.util.ServletUtil;

/**
 * This servlet provides the user's avatar image, if it is not already available
 * a default one will be generated and saved in the user's profile
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.6.1
 */
public class AvatarServlet extends HttpServlet {

	public static final String USER_ID = "userId";

	private static final long serialVersionUID = -6956612970433309888L;

	private static final String DATA_PREFIX = "data:image/";

	private static final Logger log = LoggerFactory.getLogger(AvatarServlet.class);

	/**
	 * Constructor of the object.
	 */
	public AvatarServlet() {
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
			String id = request.getParameter(USER_ID);

			String content = UserUtil.getAvatarImage(id);
			String fileName = "avatar-" + id + "." + getExtension(content);

			response.setContentType(MimeType.getByFilename(fileName));
			ServletUtil.setContentDisposition(request, response, fileName);

			saveImage(content, response.getOutputStream());
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			ServletUtil.sendError(response, e.getMessage());
		}
	}

	private String getExtension(String content) {
		String extension = "png";
		if (content.startsWith(DATA_PREFIX)) {
			extension = content.substring(content.indexOf('/') + 1, content.indexOf(';') - 1);
			if (extension.contains("+"))
				extension = extension.substring(0, extension.indexOf('+'));
		}
		return extension;
	}

	private void saveImage(String content, OutputStream out) throws IOException {
		if (content.startsWith(DATA_PREFIX))
			content = content.substring(content.indexOf(',') + 1);

		Decoder decoder = Base64.getDecoder();
		byte[] imageByte = decoder.decode(content);
		try (ByteArrayInputStream bis = new ByteArrayInputStream(imageByte)) {
			IOUtils.write(imageByte, out);
		}
	}
}