package com.logicaldoc.core;
import java.io.File;
import java.io.IOException;
import java.net.InetAddress;

import com.maxmind.geoip2.DatabaseReader;
import com.maxmind.geoip2.exception.GeoIp2Exception;
import com.maxmind.geoip2.model.CityResponse;

/**
 * To manually try the GeoIP
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class GeolocationWorkbench {

	public static void main(String[] args) throws IOException, GeoIp2Exception {
		File database = new File("C:\\Users\\marco\\Downloads\\GeoLite2-City_20201201\\GeoLite2-City.mmdb");
	    DatabaseReader dbReader = new DatabaseReader.Builder(database)
	      .build();
	    
	  System.out.println(dbReader.getMetadata().getBinaryFormatMajorVersion() +"."+dbReader.getMetadata().getBinaryFormatMinorVersion()+" ("+dbReader.getMetadata().getBuildDate()+") "+dbReader.getMetadata().getDatabaseType());
	    
	    String pollandIP="85.128.128.104";
	    String usaIP = "www.maxmind.com";
	    String italyIP="79.3.18.44";
	    String localhost="127.0.0.1";
	    
	    InetAddress ipAddress = InetAddress.getByName(localhost);
	    CityResponse response = dbReader.city(ipAddress);
	        
	    String countryName = response.getCountry().getName();
	    String countryCode = response.getCountry().getIsoCode();
	    String cityName = response.getCity().getName();
	    String postal = response.getPostal().getCode();
	    String state = response.getLeastSpecificSubdivision().getName();
	    
	    System.out.println(countryName+"("+countryCode+"), "+cityName+", "+postal+", "+state);
	}

}
