package com.logicaldoc.webservice.model;

import javax.xml.bind.annotation.XmlType;

import com.logicaldoc.webservice.doc.WSDoc;

@XmlType(name = "WSSystemInfo")
public class WSSystemInfo {

	@WSDoc(documented=false)
	protected static final long serialVersionUID = 1L;

	protected String productName = "LogicalDOC Community";

	protected String product = "LogicalDOC";

	protected String release = "8.6";

	protected String year = "2006-2021";

	protected String help = "https://docs.logicaldoc.com";

	protected String bugs = "https://bugs.logicaldoc.com";

	protected String url = "https://www.logicaldoc.com";

	protected String forum = "https://forums.logicaldoc.com";

	protected String vendor = "LogicalDOC";

	protected String vendorAddress = "via Aldo Moro interna, 3";

	protected String vendorCap = "41012";

	protected String vendorCountry = "Italy";

	protected String vendorCity = "Carpi";

	protected String support = "support@logicaldoc.com";

	protected String installationId;

	protected String licensee;

	protected String runLevel;

	protected String[] features;

	protected String date = null;
	
	protected String hostName;

	public String getProductName() {
		return productName;
	}

	public void setProductName(String productName) {
		this.productName = productName;
	}

	public String getProduct() {
		return product;
	}

	public void setProduct(String product) {
		this.product = product;
	} 

	public String getRelease() {
		return release;
	}

	public void setRelease(String release) {
		this.release = release;
	}

	public String getYear() {
		return year;
	}

	public void setYear(String year) {
		this.year = year;
	}

	public String getHelp() {
		return help;
	}

	public void setHelp(String help) {
		this.help = help;
	}

	public String getBugs() {
		return bugs;
	}

	public void setBugs(String bugs) {
		this.bugs = bugs;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getForum() {
		return forum;
	}

	public void setForum(String forum) {
		this.forum = forum;
	}

	public String getVendor() {
		return vendor;
	}

	public void setVendor(String vendor) {
		this.vendor = vendor;
	}

	public String getVendorAddress() {
		return vendorAddress;
	}

	public void setVendorAddress(String vendorAddress) {
		this.vendorAddress = vendorAddress;
	}

	public String getVendorCap() {
		return vendorCap;
	}

	public void setVendorCap(String vendorCap) {
		this.vendorCap = vendorCap;
	}

	public String getVendorCountry() {
		return vendorCountry;
	}

	public void setVendorCountry(String vendorCountry) {
		this.vendorCountry = vendorCountry;
	}

	public String getVendorCity() {
		return vendorCity;
	}

	public void setVendorCity(String vendorCity) {
		this.vendorCity = vendorCity;
	}

	public String getSupport() {
		return support;
	}

	public void setSupport(String support) {
		this.support = support;
	}

	public String getInstallationId() {
		return installationId;
	}

	public void setInstallationId(String installationId) {
		this.installationId = installationId;
	}

	public String getLicensee() {
		return licensee;
	}

	public void setLicensee(String licensee) {
		this.licensee = licensee;
	}

	public String getRunLevel() {
		return runLevel;
	}

	public void setRunLevel(String runLevel) {
		this.runLevel = runLevel;
	}

	public String[] getFeatures() {
		return features;
	}

	public void setFeatures(String[] features) {
		this.features = features;
	}

	public String getDate() {
		return date;
	}

	public void setDate(String date) {
		this.date = date;
	}

	public String getHostName() {
		return hostName;
	}

	public void setHostName(String hostName) {
		this.hostName = hostName;
	}
}