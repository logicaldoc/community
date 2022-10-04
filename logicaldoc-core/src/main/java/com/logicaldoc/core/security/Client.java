package com.logicaldoc.core.security;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A remote client connected to LogicalDOC
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class Client {

	private static Logger log = LoggerFactory.getLogger(Client.class);

	/**
	 * An optional Identifier of the client, you must guarantee that this is
	 * unique.
	 */
	private String id;

	private String username;

	private String address;

	private String host;

	private Device device;

	private Geolocation geolocation;

	public Client() {
	}

	public Client(HttpServletRequest req) {
		this(null, req.getRemoteAddr(), req.getRemoteHost());
		device = new Device(req);
	}

	public Client(String id, String address, String host) {
		super();
		this.id = id;
		this.address = address;
		this.host = host;
		try {
			this.geolocation = Geolocation.get(address);
		} catch (IOException e) {
			log.debug("Geolocalization: " + e.getMessage(), address);
		}
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null)
			return false;

		if (this.getClass() != obj.getClass())
			return false;

		Client other = (Client) obj;
		if ((id == null && other.id != null) || (id != null && other.id == null))
			return false;

		return id != null ? id.equals(other.id) : false;
	}

	@Override
	public int hashCode() {
		return id.hashCode();
	}

	@Override
	public String toString() {
		if (id == null)
			return String.format("%s - %s", host, address);
		else
			return String.format("%s - %s - %s", id, host, address);
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public Device getDevice() {
		return device;
	}

	public void setDevice(Device device) {
		this.device = device;
	}

	public Geolocation getGeolocation() {
		return geolocation;
	}

	public void setGeolocation(Geolocation geolocation) {
		this.geolocation = geolocation;
	}
}