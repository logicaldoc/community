package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.TimeZone;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.dao.DeviceDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.web.util.ServiceUtil;

/**
 * This servlet retrieves the trusted devices
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class DevicesDataServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private static Logger log = LoggerFactory.getLogger(DevicesDataServlet.class);

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

			boolean trustedOnly = request.getParameter("trustedonly") != null;

			PrintWriter writer = response.getWriter();
			writer.write("<list>");

			DeviceDAO dDao = (DeviceDAO) Context.get().getBean(DeviceDAO.class);
			List<Device> devices = new ArrayList<Device>();
			if (trustedOnly)
				devices = dDao.findTrustedDevices(session.getUserId());
			else
				devices = dDao.findByUserId(session.getUserId());

			DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
			df.setTimeZone(TimeZone.getTimeZone("UTC"));

			/*
			 * Iterate over records composing the response XML document
			 */
			for (Device device : devices) {
				if (device.getDeleted() == 1)
					continue;

				writer.print("<device>");
				writer.print("<id>" + device.getId() + "</id>");
				if (device.getLabel() != null)
					writer.print("<label><![CDATA[" + device.getLabel() + "]]></label>");
				writer.print("<deviceId>" + device.getDeviceId() + "</deviceId>");
				if (device.getBrowser() != null)
					writer.print("<browser><![CDATA[" + device.getBrowser() + " v" + device.getBrowserVersion()
							+ "]]></browser>");
				if (device.getOperativeSystem() != null)
					writer.print("<os><![CDATA[" + device.getOperativeSystem() + "]]></os>");
				if (device.getType() != null)
					writer.print("<type>" + device.getType() + "</type>");
				if (device.getCreation() != null)
					writer.print("<creation>" + df.format(device.getCreation()) + "</creation>");
				if (device.getLastLogin() != null)
					writer.print("<lastlogin>" + df.format(device.getLastLogin()) + "</lastlogin>");
				writer.print("</device>");
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
