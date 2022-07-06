package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * GUI representation of a Key Store
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class GUIKeystore implements Serializable {

	private static final long serialVersionUID = 1L;

	private long id;

	private long tenantId;

	private int validity = 2;

	private String organizationAlias = "root";

	private String organizationDN = "";

	private Date organizationExpire = new Date();

	private String password = "";

	private Date created = new Date();

	private String keytoolPath = "";

	private String openSSLPath = "";

	private boolean signVisual = false;

	private String signX;

	private String signY;
	
	private String signWidth="$PAGE_WIDTH/6";

	private int signOpacity = 100;

	private int signFontSize = 6;

	private String signFontColor = "#000000";

	public String getOrganizationAlias() {
		return organizationAlias;
	}

	public void setOrganizationAlias(String organizationAlias) {
		this.organizationAlias = organizationAlias;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public Date getCreated() {
		return created;
	}

	public void setCreated(Date created) {
		this.created = created;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public long getTenantId() {
		return tenantId;
	}

	public void setTenantId(long tenantId) {
		this.tenantId = tenantId;
	}

	public String getOpenSSLPath() {
		return openSSLPath;
	}

	public void setOpenSSLPath(String openSSLPath) {
		this.openSSLPath = openSSLPath;
	}

	public String getKeytoolPath() {
		return keytoolPath;
	}

	public void setKeytoolPath(String keytoolPath) {
		this.keytoolPath = keytoolPath;
	}

	public String getOrganizationDN() {
		return organizationDN;
	}

	public void setOrganizationDN(String organizationDN) {
		this.organizationDN = organizationDN;
	}

	public int getValidity() {
		return validity;
	}

	public void setValidity(int validity) {
		this.validity = validity;
	}

	public Date getOrganizationExpire() {
		return organizationExpire;
	}

	public void setOrganizationExpire(Date organizationExpire) {
		this.organizationExpire = organizationExpire;
	}

	public boolean isSignVisual() {
		return signVisual;
	}

	public void setSignVisual(boolean signVisual) {
		this.signVisual = signVisual;
	}

	public String getSignX() {
		return signX;
	}

	public void setSignX(String signX) {
		this.signX = signX;
	}

	public String getSignY() {
		return signY;
	}

	public void setSignY(String signY) {
		this.signY = signY;
	}

	public int getSignOpacity() {
		return signOpacity;
	}

	public void setSignOpacity(int signOpacity) {
		this.signOpacity = signOpacity;
	}

	public int getSignFontSize() {
		return signFontSize;
	}

	public void setSignFontSize(int signFontSize) {
		this.signFontSize = signFontSize;
	}

	public String getSignFontColor() {
		return signFontColor;
	}

	public void setSignFontColor(String signFontColor) {
		this.signFontColor = signFontColor;
	}

	public String getSignWidth() {
		return signWidth;
	}

	public void setSignWidth(String signWidth) {
		this.signWidth = signWidth;
	}
}