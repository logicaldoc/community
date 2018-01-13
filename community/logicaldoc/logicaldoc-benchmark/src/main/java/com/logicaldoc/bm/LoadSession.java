package com.logicaldoc.bm;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.StringTokenizer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.config.ContextProperties;

/**
 * Models a load session during which the LogicalDOC server will be heavy
 * loaded.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 6.5
 */
public class LoadSession {

	private static Logger log = LoggerFactory.getLogger(LoadSession.class);

	public static final ThreadGroup THREAD_GROUP = new ThreadGroup("FileFolderRemoteLoader");

	private String username;

	private String password;

	private String url;

	private String language = "en";

	private OutputStream outSummary;

	private long startTime;

	private List<String> urls = new ArrayList<String>();

	private Random random = new Random();

	private List<AbstractServerProxy> servers = new ArrayList<AbstractServerProxy>();

	private static String wsAPI;

	public LoadSession() {

	}

	public AbstractServerProxy getRemoteServer() {
		if (servers != null && servers.size() > 0) {
			int idx = random.nextInt(servers.size());
			return servers.get(idx);
		}
		return null;
	}

	/**
	 * Connects to the server before first use.
	 */
	public synchronized void connect() throws Exception {
		StringTokenizer tokenizer = new StringTokenizer(url, ",", false);
		while (tokenizer.hasMoreTokens()) {
			String s = tokenizer.nextToken().trim();
			AbstractServerProxy server = LoadSession.connect(s, username, password);
			servers.add(server);
			log.info("Connected to server {}", s);
		}

		// Construct output and error files
		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd_HHmmss");

		File fileSummary = new File("report");
		fileSummary.mkdir();
		fileSummary = new File(fileSummary + "/LoadSession-" + df.format(new Date()) + ".tsv");
		outSummary = new BufferedOutputStream(new FileOutputStream(fileSummary));

		// Record the start time
		startTime = System.currentTimeMillis();
	}

	public synchronized void close() {
		System.out.println("LoadSession.close");
		int counter = 0;
		for (AbstractServerProxy server : servers) {
			counter++;
			System.out.println("logout server: " + counter);
			server.logout();
			System.out.println("logout complete for server: " + counter);
		}
	}

	private static AbstractServerProxy connect(String url, String username, String password) throws Exception {
		log.info("Connect to the server");

		AbstractServerProxy remoteServer = null;

		try {
			ContextProperties config = new ContextProperties();

			// Store the service references
			AbstractServerProxy lsp = null;
			wsAPI = config.getProperty("serverproxy.type");
			if ("rest".equals(wsAPI)) {				
				lsp = new RestServerProxy(url, config);
			} else {
				lsp = new SoapServerProxy(url, config);
			}

			// Authenticate
			String ticket = lsp.login(username, password);
			log.info("Created SID: {}", ticket);

			remoteServer = lsp;
		} catch (Throwable e) {
			log.error("Unable to initialize WebServices connection", e);
		}

		// Check that there is at least one server
		if (remoteServer == null) {
			throw new Exception("Remote server " + url + " not available");
		}
		return remoteServer;
	}

	public static String getLineEnding() {
		return System.getProperty("line.separator", "\n");
	}

	public synchronized void logSummary(String msg) {
		if (outSummary == null) {
			return;
		}
		try {
			byte[] bytes = msg.getBytes("UTF-8");
			outSummary.write(bytes);
			outSummary.write(getLineEnding().getBytes("UTF-8"));
			outSummary.flush();
		} catch (Throwable e) {
			System.err.println("Failed to write message to summary file: " + e.getMessage());
		}
	}

	public String getSummary() {
		StringBuilder sb = new StringBuilder();
		sb.append("Server URL:         ").append(url).append(getLineEnding()).append(getLineEnding())
	      .append("Service type:     ").append(wsAPI).append(getLineEnding()).append(getLineEnding())
		  .append("Start Time:       ").append(new Date(startTime));
		return sb.toString();
	}

	public String getLanguage() {
		return language;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String urls) {
		this.url = urls;
	}

	public void setLanguage(String language) {
		this.language = language;
	}
}