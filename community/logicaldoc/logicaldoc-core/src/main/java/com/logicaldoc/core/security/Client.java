package com.logicaldoc.core.security;

/**
 * A remote client connected to LogicalDOC
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class Client {

	/**
	 * An optional Identifier of the client, you must guarantee that this is
	 * unique.
	 */
	private String id;

	private String address;

	private String host;

	public Client() {
	}

	public Client(String address, String host) {
		this(null, address, host);
	}

	public Client(String id, String address, String host) {
		super();
		this.id = id;
		this.address = address;
		this.host = host;
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

		Client other = (Client) obj;
		if ((id == null && other.id != null) || (id != null && other.id == null))
			return false;

		return id.equals(other.id);
	}

	@Override
	public String toString() {
		if (id == null)
			return String.format("%s - %s", host, address);
		else
			return String.format("%s - %s - %s", id, host, address);
	}
}