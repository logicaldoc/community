package com.logicaldoc.webservice.model;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.webservice.doc.WSDoc;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(name = "systeminfo")
@XmlType(name = "WSSystemInfo")
public class WSSystemInfo {

	@WSDoc(documented = false)
	protected static final long serialVersionUID = 1L;

	@WSDoc(required = true)
	private String productName = "LogicalDOC Community";

	@WSDoc(required = true)
	private String product = "LogicalDOC";

	@WSDoc(required = true)
	private String release = "8.8.3";

	@WSDoc(required = true)
	private String year = "2006-2022";

	@WSDoc(required = true)
	private String help = "https://docs.logicaldoc.com";

	@WSDoc(required = true)
	private String bugs = "https://bugs.logicaldoc.com";

	@WSDoc(required = true)
	private String url = "https://www.logicaldoc.com";

	@WSDoc(required = true)
	private String forum = "https://forums.logicaldoc.com";

	@WSDoc(required = true)
	private String vendor = "LogicalDOC";

	@WSDoc(required = true)
	private String vendorAddress = "via Aldo Moro interna, 3";

	@WSDoc(required = true)
	private String vendorCap = "41012";

	@WSDoc(required = true)
	private String vendorCountry = "Italy";

	@WSDoc(required = true)
	private String vendorCity = "Carpi";

	@WSDoc(required = true)
	private String support = "support@logicaldoc.com";

	@WSDoc(required = true)
	private String installationId;

	@WSDoc(required = true)
	private String licensee;

	@WSDoc(required = true)
	private String runLevel;

	/**
	 * Deducted server's host name
	 */
	@WSDoc(required = true)
	private String hostName;

	@WSDoc(required = true)
	private List<String> features = new ArrayList<>();

	@WSDoc(required = true)
	private String date = null;

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

	public List<String> getFeatures() {
		return features;
	}

	public void setFeatures(List<String> features) {
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