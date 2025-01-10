package com.logicaldoc.webserviceclient;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;

import org.ksoap2.SoapEnvelope;
import org.ksoap2.serialization.MarshalBase64;
import org.ksoap2.serialization.PropertyInfo;
import org.ksoap2.serialization.SoapObject;
import org.ksoap2.serialization.SoapSerializationEnvelope;
import org.ksoap2.transport.HttpTransportSE;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmlpull.v1.XmlPullParserException;

/**
 * Facade for the connection with the remote LogicalDOC server. It uses basic
 * SOAP calls using KSoap
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public class WebserviceClient {

	private static final String DOC_ID = "docId";

	private static final String SERVICES_DOCUMENT = "services/Document";

	private static final String INVOKING = "Invoking {}";

	private static Logger log = LoggerFactory.getLogger(WebserviceClient.class);

	private static final String WS_NAMESPACE = "http://ws.logicaldoc.com";

	private static final int SOAP_VERSION = SoapEnvelope.VER11;

	private static WebserviceClient instance;

	private String sid;

	private String baseUrl;

	private int timeout = 120000;

	public static WebserviceClient get(String sid, String baseUrl) {
		if (instance == null) {
			log.info("Connect to remote server {}", baseUrl);
			instance = new WebserviceClient();
			instance.sid = sid;
			instance.baseUrl = baseUrl;
		}
		return instance;
	}

	public static WebserviceClient get() {
		return instance;
	}

	public String getPath(long folderId) throws IOException, XmlPullParserException {
		SoapObject request = new SoapObject(WS_NAMESPACE, "getPath");
		request.addProperty("sid", sid);
		request.addProperty("folderId", folderId);
		SoapSerializationEnvelope envelope = new SoapSerializationEnvelope(SOAP_VERSION);
		envelope.setOutputSoapObject(request);

		String url = baseUrl + (baseUrl.endsWith("/") ? "" : "/") + "services/Folder";
		log.debug(INVOKING, url);

		HttpTransportSE transport = new HttpTransportSE(url);
		transport.call("", envelope);

		AbstractList<SoapObject> response = new ArrayList<>();
		
		if (envelope.getResponse() instanceof List) {
			// We have more elements
			@SuppressWarnings("unchecked")
			List<SoapObject> vector = (List<SoapObject>) envelope.getResponse();
			for (SoapObject soapObject : vector) {
				response.add(soapObject);
			}
		} else if (envelope.getResponse() instanceof SoapObject soapObject) {
			// We have just one element
			response.add(soapObject);
		}

		StringBuilder path = new StringBuilder();

		for (SoapObject element : response) {
			String name = getProperty(element, "name");
			if (!"/".equals(name))
				path.append(name);
			path.append("/");
		}

		String ret = path.toString();
		if (ret.endsWith("/"))
			ret = ret.substring(0, ret.length() - 1);
		return ret;
	}

	public void uploadFile(long targetDir, File file, String fileName, String language)
			throws IOException, XmlPullParserException {
		SoapObject request = new SoapObject(WS_NAMESPACE, "create");

		request.addProperty("sid", sid);

		SoapObject document = new SoapObject("", "document");
		request.addProperty("document", document);

		document.addProperty("fileName", fileName);
		document.addProperty("folderId", targetDir);
		document.addProperty("language", language);

		MarshalBase64 marshal = new MarshalBase64();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		write(file, out);
		byte[] raw = out.toByteArray();
		request.addProperty("content", raw);

		SoapSerializationEnvelope envelope = new SoapSerializationEnvelope(SOAP_VERSION);
		envelope.implicitTypes = true;
		envelope.setOutputSoapObject(request);

		marshal.register(envelope);

		String url = baseUrl + (baseUrl.endsWith("/") ? "" : "/") + SERVICES_DOCUMENT;
		log.debug(INVOKING, url);
		HttpTransportSE transport = new HttpTransportSE(url);

		// Use an empty SOAPAction or CXF will be not able to parse
		transport.call("", envelope, null);

		envelope.getResponse();
		marshal.register(envelope);
	}

	public String getBaseUrl() {
		return baseUrl;
	}

	public int getTimeout() {
		return timeout;
	}

	private String getProperty(SoapObject object, String name) {
		if (object == null)
			return null;
		PropertyInfo pinfo = new PropertyInfo();
		for (int i = 0; i < object.getPropertyCount(); i++) {
			object.getPropertyInfo(i, pinfo);
			if (pinfo.getName().equals(name)) {
				String val = object.getProperty(i).toString();
				if (!"anyType{}".equals(val) && val != null)
					return val;
				else
					return null;
			}
		}
		return null;
	}

	public Entry getDocument(long id) throws IOException, XmlPullParserException {
		SoapObject request = new SoapObject(WS_NAMESPACE, "getDocument");
		request.addProperty("sid", sid);
		request.addProperty(DOC_ID, Long.toString(id));

		SoapSerializationEnvelope envelope = new SoapSerializationEnvelope(SOAP_VERSION);
		envelope.setOutputSoapObject(request);

		String url = baseUrl + (baseUrl.endsWith("/") ? "" : "/") + SERVICES_DOCUMENT;
		log.info(INVOKING, url);
		HttpTransportSE transport = new HttpTransportSE(url, timeout);
		transport.call("", envelope);

		SoapObject response = (SoapObject) envelope.getResponse();
		Entry document = new Entry(Long.parseLong(getProperty(response, "id")));
		document.setFileName(getProperty(response, "fileName"));
		document.setVersion(getProperty(response, "version"));

		return document;
	}

	public void checkin(long docId, File file) throws IOException, XmlPullParserException {
		SoapObject request = new SoapObject(WS_NAMESPACE, "checkin");
		request.addProperty("sid", sid);
		request.addProperty(DOC_ID, docId);
		request.addProperty("filename", file.getName());
		request.addProperty("release", false);

		MarshalBase64 marshal = new MarshalBase64();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		write(file, out);
		byte[] raw = out.toByteArray();
		request.addProperty("content", raw);

		String url = baseUrl + (baseUrl.endsWith("/") ? "" : "/") + SERVICES_DOCUMENT;

		SoapSerializationEnvelope envelope = new SoapSerializationEnvelope(SOAP_VERSION);
		envelope.setOutputSoapObject(request);
		marshal.register(envelope);

		HttpTransportSE transport = new HttpTransportSE(url + "/services/Document", timeout);
		transport.call("", envelope);

		envelope.getResponse();
	}

	public void checkout(long docId) throws IOException, XmlPullParserException {
		SoapObject request = new SoapObject(WS_NAMESPACE, "checkout");
		request.addProperty("sid", sid);
		request.addProperty(DOC_ID, docId);

		SoapSerializationEnvelope envelope = new SoapSerializationEnvelope(SOAP_VERSION);
		envelope.setOutputSoapObject(request);

		String url = baseUrl + (baseUrl.endsWith("/") ? "" : "/") + SERVICES_DOCUMENT;
		log.info(INVOKING, url);
		HttpTransportSE transport = new HttpTransportSE(url, timeout);
		transport.call("", envelope);
	}

	public void unlock(long docId) throws IOException, XmlPullParserException {
		SoapObject request = new SoapObject(WS_NAMESPACE, "unlock");
		request.addProperty("sid", sid);
		request.addProperty(DOC_ID, docId);

		SoapSerializationEnvelope envelope = new SoapSerializationEnvelope(SOAP_VERSION);
		envelope.setOutputSoapObject(request);

		String url = baseUrl + (baseUrl.endsWith("/") ? "" : "/") + SERVICES_DOCUMENT;
		log.info(INVOKING, url);
		HttpTransportSE transport = new HttpTransportSE(url, timeout);
		transport.call("", envelope);
	}

	private void write(File in, OutputStream out) throws IOException {
		try (InputStream is = new FileInputStream(in)) {
			// initialize
			byte[] buffer = new byte[2048]; // tweaking this number may increase
											// performance
			int len;
			while ((len = is.read(buffer)) != -1) {
				out.write(buffer, 0, len);
			}
		} finally {
			if (out != null) {
				out.flush();
				out.close();
			}
		}
	}
}