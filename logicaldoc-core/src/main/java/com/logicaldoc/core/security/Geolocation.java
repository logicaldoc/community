package com.logicaldoc.core.security;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.SimpleDateFormat;
import java.util.stream.Stream;

import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;
import com.logicaldoc.util.http.FileHttpClientResponseHandler;
import com.logicaldoc.util.http.HttpUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;
import com.logicaldoc.util.plugin.PluginRegistry;
import com.maxmind.db.CHMCache;
import com.maxmind.geoip2.DatabaseReader;
import com.maxmind.geoip2.exception.GeoIp2Exception;
import com.maxmind.geoip2.model.CityResponse;

/**
 * A class to concentrate the Geolocation operations and at the same time a
 * factory to create beans that carry the geolocation of an IP address.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class Geolocation {

	private static final String CONST_GEOLOCATION = "geolocation";

	private static Logger log = LoggerFactory.getLogger(Geolocation.class);

	private String countryCode;

	private String country;

	private String city;

	private String postal;

	private String state;

	private static Geolocation singleton;

	private DatabaseReader dbReader;

	private Geolocation() {
		// Empty constructor
	}

	/**
	 * Retrieve the singleton instance of the Geolocation
	 * 
	 * @return the Geolocation singleton instance
	 */
	public static Geolocation get() {
		if (singleton == null)
			singleton = new Geolocation();
		return singleton;
	}

	/**
	 * Instantiates a new Geolocation for a given IP
	 * 
	 * @param ip the address to process
	 * 
	 * @return the IP geolocalization
	 * 
	 * @throws IOException if an error happens reading the database
	 */
	public static Geolocation get(String ip) throws IOException {
		return get().geolocate(ip);
	}

	public boolean isEnabled() {
		return Context.get().getProperties().getBoolean("security.geolocation.enabled", false);
	}

	/**
	 * Used the database to geolocalize a given IP
	 * 
	 * @param ip the address to localize
	 * 
	 * @return a Geolocation instance
	 * 
	 * @throws IOException if an error happens reading the database
	 */
	private Geolocation geolocate(String ip) throws IOException {
		if (!isEnabled())
			return null;

		get().initDB();
		if (get().dbReader == null)
			return null;

		InetAddress ipAddress = InetAddress.getByName(ip);
		try {
			CityResponse response = get().dbReader.city(ipAddress);

			Geolocation geo = new Geolocation(response.getCountry().getName(), response.getCountry().getIsoCode(),
					response.getCity().getName(), response.getPostal().getCode(),
					response.getLeastSpecificSubdivision().getName());

			log.debug("IP {} is located at {}", ip, geo);

			return geo;
		} catch (GeoIp2Exception e) {
			throw new IOException(e.getMessage(), e);
		}
	}

	/**
	 * Retrieves the version of the Geolocation database
	 * 
	 * @return The version
	 */
	public String getDatabaseVersion() {
		SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd");
		File database = getDatabaseFile();
		if (!database.exists())
			return null;

		try (DatabaseReader reader = new DatabaseReader.Builder(database).build();) {
			return df.format(reader.getMetadata().getBuildDate()) + " - " + reader.getMetadata().getDatabaseType();
		} catch (IOException e) {
			log.warn("Cannot read the version of database {}", database.getPath());
			return null;
		}
	}

	/**
	 * Downloads and installs the latest database
	 * 
	 * @param key the API key
	 * 
	 * @throws IOException in case of whatever problem
	 */
	public void syncDB(String key) throws IOException {
		File gzFile = null;
		File tmpDir = null;
		try {
			String downloadUrl = "https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-City&license_key=YOUR_LICENSE_KEY&suffix=tar.gz";
			downloadUrl = downloadUrl.replace("YOUR_LICENSE_KEY", key);

			log.info("Downloading geolocation database {}", downloadUrl);

			try (CloseableHttpClient httpclient = HttpUtil.getNotValidatingClient(60);) {
				gzFile = FileUtil.createTempFile(CONST_GEOLOCATION, ".tar.gz");
				httpclient.execute(new HttpGet(downloadUrl), new FileHttpClientResponseHandler(gzFile));
			}

			log.info("Downloaded geolocation database {}", gzFile.getPath());

			/*
			 * Prepare a temporary folder and hunzip the downloaded file in it
			 */
			tmpDir = FileUtil.createTempFile(CONST_GEOLOCATION, null);
			FileUtil.delete(tmpDir);
			tmpDir.mkdir();
			try (ZipUtil zipUtil = new ZipUtil()) {
				zipUtil.unGZipUnTar(gzFile, tmpDir);
			}

			// Search for the .mmdb file
			File mmdbFile = null;
			try (Stream<Path> paths = Files.walk(tmpDir.toPath())) {
				mmdbFile = paths.filter(p -> p.toFile().isFile() && p.toFile().getName().endsWith(".mmdb")).toList()
						.get(0).toFile();
			}

			if (!mmdbFile.exists())
				throw new IOException("Database file not found");

			FileUtil.copyFile(mmdbFile, getDatabaseFile());

			FileUtil.delete(mmdbFile);

			dispose();
		} finally {
			FileUtil.delete(gzFile);
			FileUtil.delete(tmpDir);
		}
	}

	private void initDB() throws IOException {
		File database = getDatabaseFile();
		if (!database.exists() || database.length() <= 0L || dbReader != null)
			return;
		log.info("Try to read the geolocation database {}", database);

		if (Context.get().getProperties().getBoolean("security.geolocation.cache", false))
			dbReader = new DatabaseReader.Builder(database).withCache(new CHMCache()).build();
		else
			dbReader = new DatabaseReader.Builder(database).build();
	}

	private Geolocation(String country, String countryCode, String city, String postal, String state) {
		super();
		this.country = country;
		this.countryCode = countryCode;
		this.city = city;
		this.postal = postal;
		this.state = state;
	}

	public String getCountry() {
		return country;
	}

	public String getCity() {
		return city;
	}

	public String getPostal() {
		return postal;
	}

	public String getState() {
		return state;
	}

	public String getCountryCode() {
		return countryCode;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (city != null)
			sb.append(city);

		if (state != null) {
			if (sb.length() > 0)
				sb.append(", ");
			sb.append(state);
		}

		if (country != null) {
			if (sb.length() > 0) {
				sb.append(" (");
				sb.append(countryCode);
				sb.append(")");
			} else {
				sb.append(country);
			}
		}

		return sb.toString();
	}

	private static File getDatabaseFile() {
		File folder = PluginRegistry.getPluginResource("logicaldoc-core", CONST_GEOLOCATION);
		return new File(folder, "geolocation.db");
	}

	public void dispose() {
		if (dbReader != null) {
			try {
				dbReader.close();
			} catch (Exception e) {
				// Nothing to do
			}
			dbReader = null;
		}
	}
}