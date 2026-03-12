package com.logicaldoc.web.data;

import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.util.List;
import java.util.Locale;

import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.DeviceDAO;
import com.logicaldoc.core.security.Session;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

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
			writer.print(String.format("<id>%d</id>", device.getId()));

			printLabel(device, writer);

			writer.print(String.format("<deviceId>%s</deviceId>", device.getDeviceId()));

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
			writer.print(String.format("<type>%s</type>", device.getType()));
	}

	private void printLabel(Device device, PrintWriter writer) {
		if (device.getLabel() != null)
			writer.print(String.format("<label><![CDATA[%s]]></label>", device.getLabel()));
	}

	private void printOperativeSystem(Device device, PrintWriter writer) {
		if (device.getOperativeSystem() != null)
			writer.print(String.format("<os><![CDATA[%s]]></os>", device.getOperativeSystem()));
	}

	private void printBrowser(Device device, PrintWriter writer) {
		if (device.getBrowser() != null)
			writer.print(String.format("<browser><![CDATA[%s v%s]]></browser>", device.getBrowser(),
					device.getBrowserVersion()));
	}

	private List<Device> getDevices(Session session, boolean trustedOnly) {
		List<Device> devices;
		if (trustedOnly)
			devices = DeviceDAO.get().findTrustedDevices(session.getUserId());
		else
			devices = DeviceDAO.get().findByUserId(session.getUserId());
		return devices;
	}

	private void printDates(Device device, PrintWriter writer) {
		DateFormat df = getDateFormat();
		if (device.getCreation() != null)
			writer.print(String.format("<creation>%s</creation>", df.format(device.getCreation())));
		if (device.getLastLogin() != null)
			writer.print(String.format("<lastlogin>%s</lastlogin>", df.format(device.getLastLogin())));
	}
}
