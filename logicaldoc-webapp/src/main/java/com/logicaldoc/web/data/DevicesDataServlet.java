package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.dao.DeviceDAO;
import com.logicaldoc.util.Context;

/**
 * This servlet retrieves the trusted devices
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class DevicesDataServlet extends AbstractDataServlet {

	private static final long serialVersionUID = 1L;

	@Override
	protected void service(HttpServletRequest request, HttpServletResponse response, Session session, Integer max,
			Locale locale) throws IOException {

		boolean trustedOnly = request.getParameter("trustedonly") != null;

		PrintWriter writer = response.getWriter();
		writer.write("<list>");

		List<Device> devices = getDevices(session, trustedOnly);

		/*
		 * Iterate over records composing the response XML document
		 */
		for (Device device : devices) {
			if (device.getDeleted() == 1)
				continue;

			writer.print("<device>");

			writer.print("<id>" + device.getId() + "</id>");

			printLabel(device, writer);

			writer.print("<deviceId>" + device.getDeviceId() + "</deviceId>");

			printBrowser(device, writer);

			printOperativeSystem(device, writer);

			printType(device, writer);

			printDates(device, writer);

			writer.print("</device>");
		}

		writer.write("</list>");
	}

	private void printType(Device device, PrintWriter writer) {
		if (device.getType() != null)
			writer.print("<type>" + device.getType() + "</type>");
	}

	private void printLabel(Device device, PrintWriter writer) {
		if (device.getLabel() != null)
			writer.print("<label><![CDATA[" + device.getLabel() + "]]></label>");
	}

	private void printOperativeSystem(Device device, PrintWriter writer) {
		if (device.getOperativeSystem() != null)
			writer.print("<os><![CDATA[" + device.getOperativeSystem() + "]]></os>");
	}

	private void printBrowser(Device device, PrintWriter writer) {
		if (device.getBrowser() != null)
			writer.print(
					"<browser><![CDATA[" + device.getBrowser() + " v" + device.getBrowserVersion() + "]]></browser>");
	}

	private List<Device> getDevices(Session session, boolean trustedOnly) {
		DeviceDAO dDao = (DeviceDAO) Context.get().getBean(DeviceDAO.class);
		List<Device> devices = new ArrayList<Device>();
		if (trustedOnly)
			devices = dDao.findTrustedDevices(session.getUserId());
		else
			devices = dDao.findByUserId(session.getUserId());
		return devices;
	}

	private void printDates(Device device, PrintWriter writer) {
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
		df.setTimeZone(TimeZone.getTimeZone("UTC"));
		if (device.getCreation() != null)
			writer.print("<creation>" + df.format(device.getCreation()) + "</creation>");
		if (device.getLastLogin() != null)
			writer.print("<lastlogin>" + df.format(device.getLastLogin()) + "</lastlogin>");
	}
}
