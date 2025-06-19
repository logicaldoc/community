package com.logicaldoc.core;

import java.util.List;

/**
 * Just a development utility to produce the scripts to reset the sequences
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 *
 */
public class PrepareSequencesReset {
	private final static List<String> tables = List.of("ld_bookmark", "ld_document", "ld_generic", "ld_group",
			"ld_history", "ld_link", "ld_menu", "ld_systemmessage", "ld_template", "ld_attributeset", "ld_ticket",
			"ld_user", "ld_user_history", "ld_version", "ld_folder", "ld_folder_history", "ld_rating", "ld_note",
			"ld_messagetemplate", "ld_contact", "ld_tenant", "ld_sequence", "ld_extoption", "ld_session", "ld_dashlet",
			"ld_device", "ld_password_history", "ld_search", "ld_apikey", "ld_bookmark", "ld_webservicecall");

	public static void main(String[] args) {
		resetMariaDB();
		
		resetMySQL();
	}

	private static void resetMariaDB() {
		System.out.println("\n\n--------------\nMariaDB:\n");

		for (String table : tables) {
			System.out.println("select max(ld_id) + 50  into @rst from " + table + ";");
			System.out.println("EXECUTE IMMEDIATE CONCAT('alter sequence " + table + "_SEQ RESTART WITH ', (@rst));");
		}

		System.out.println("\n--------------\n");
	}

	private static void resetMySQL() {
		System.out.println("\n\n--------------\nMySQL:\n");

		for (String table : tables) 
			System.out.println("delete from " + table + "_SEQ");
		System.out.println(" ");
		for (String table : tables) 	
			System.out.println("insert into " + table + "_SEQ(next_val) select max(ld_id) + 50 from " + table + ";");

		System.out.println("\n--------------\n");
	}
}
