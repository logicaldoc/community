package com.logicaldoc.core;

import java.util.Set;
import java.util.stream.Collectors;

/**
 * Just a development utility to produce the scripts to reset the sequences
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.1
 *
 */
public class SequencesWorkbench {
	private final static Set<String> tables = Set.of("ld_bookmark", "ld_document", "ld_generic", "ld_group",
			"ld_history", "ld_link", "ld_menu", "ld_systemmessage", "ld_template", "ld_attributeset", "ld_ticket",
			"ld_user", "ld_user_history", "ld_version", "ld_folder", "ld_folder_history", "ld_rating", "ld_note",
			"ld_messagetemplate", "ld_contact", "ld_tenant", "ld_sequence", "ld_extoption", "ld_session", "ld_dashlet",
			"ld_device", "ld_password_history", "ld_search", "ld_apikey", "ld_webservicecall", "ld_aimodel",
			"ld_aimodel_history", "ld_sampler", "ld_audit", "ld_barcodetemplate", "ld_event", "ld_event_ref",
			"ld_chatmessage", "ld_emailaccount", "ld_automation_routine", "ld_automation_trigger", "ld_branding",
			"ld_readingrequest", "ld_form", "ld_archive", "ld_importfolder", "ld_importfolder_history", "ld_ldapserver",
			"ld_ocr_history", "ld_report", "ld_retentionpolicy", "ld_robot", "ld_robot_history", "ld_keystore",
			"ld_stamp", "ld_syndication", "ld_workflowtemplate", "ld_workflowhistory", "ld_workflowtrigger",
			"ld_ocrtemplate");

	public static void main(String[] args) {
		System.out.println("total tables: " + tables.size());
		System.out.println(tables.stream().collect(Collectors.joining(",")));

		// resetMariaDB();
		resetMySQL();
		// resetHSQLDB();
		// resetSqlServer();
		// resetOracle();
		// resetPostreSQL();

//		createMariaDB();
//		createHSQLDB();
//		createSqlServer();
//		createOracle();
//		createPostreSQL();
	}

	private static void createPostreSQL() {
		System.out.println("\n\n--------------\nPostreSQL:\n");

		for (String table : tables) {
			System.out
					.println("create sequence " + table + "_SEQ  increment by 50 start with START_" + table + "_SEQ;");
		}

		System.out.println("\n--------------\n");
	}

	private static void createOracle() {
		System.out.println("\n\n--------------\nOracle:\n");

		for (String table : tables) {
			System.out
					.println("create sequence " + table + "_SEQ  start with START_" + table + "_SEQ increment by 50;");
		}

		System.out.println("\n--------------\n");
	}

	private static void createSqlServer() {
		System.out.println("\n\n--------------\nSQL Server:\n");

		for (String table : tables) {
			System.out
					.println("create sequence " + table + "_SEQ  start with START_" + table + "_SEQ increment by 50;");
		}

		System.out.println("\n--------------\n");
	}

	private static void createHSQLDB() {
		System.out.println("\n\n--------------\nMariaDB:\n");

		for (String table : tables) {
			System.out
					.println("create sequence " + table + "_SEQ  start with START_" + table + "_SEQ increment by 50;");
		}

		System.out.println("\n--------------\n");
	}

	private static void createMariaDB() {
		System.out.println("\n\n--------------\nMariaDB:\n");

		for (String table : tables) {
			System.out
					.println("create sequence " + table + "_SEQ  increment by 50 start with START_" + table + "_SEQ;");
		}

		System.out.println("\n--------------\n");
	}

	private static void resetPostreSQL() {
		System.out.println("\n\n--------------\nPostgreSQL:\n");

		for (String table : tables) {
			System.out.println("""
DO
$$
DECLARE
  rst bigint
BEGIN""");
			System.out.println(" select coalesce(max(ld_id),0) + 50  into rst from " + table + ";");
			System.out.println("  alter sequence " + table + "_SEQ RESTART WITH rst;");
			System.out.println("""
END;
$$""");
			System.out.println("");
		}

		System.out.println("\n--------------\n");
	}

	private static void resetOracle() {
		System.out.println("\n\n--------------\nOracle:\n");

		for (String table : tables) {
			System.out.println("declare\n  rst number(19,0);");
			System.out.println("begin");
			System.out.println("  select nvl(max(ld_id),0) + 50  into rst from " + table + ";");
			System.out.println("  alter sequence " + table + "_SEQ RESTART START WITH rst;");
			System.out.println("end;");
			System.out.println("");
		}

		System.out.println("\n--------------\n");
	}

	private static void resetSqlServer() {
		System.out.println("\n\n--------------\nSQL Server:\n");

		for (String table : tables) {
			System.out.println("DECLARE @rst bigint;");
			System.out.println("select @rst = isnull(max(ld_id),0) + 50  from " + table + ";");
			System.out.println("alter sequence " + table + "_SEQ RESTART WITH @rst;");
			System.out.println("");
		}

		System.out.println("\n--------------\n");
	}

	private static void resetHSQLDB() {
		System.out.println("\n\n--------------\nHSQLDB:\n");

		for (String table : tables) {
			System.out.println("* rst ~");
			System.out.println("select ifnull(max(ld_id),0) + 50 from " + table + ";");
			System.out.println("alter sequence " + table + "_SEQ RESTART WITH *{rst};");
			System.out.println("");
		}

		System.out.println("\n--------------\n");
	}

	private static void resetMariaDB() {
		System.out.println("\n\n--------------\nMariaDB:\n");

		for (String table : tables) {
			System.out.println("select ifnull(max(ld_id),0) + 50  into @rst from " + table + ";");
			System.out.println("EXECUTE IMMEDIATE CONCAT('alter sequence " + table + "_SEQ RESTART WITH ', (@rst));");
		}

		System.out.println("\n--------------\n");
	}

	private static void resetMySQL() {
		System.out.println("\n\n--------------\nMySQL:\n");

		for (String table : tables)
			System.out.println("delete from " + table + "_SEQ;");
		System.out.println(" ");
		for (String table : tables)
			System.out.println("insert into " + table + "_SEQ select max(next_val) + 50 from ld_hilo where ld_sequence= '" + table + "';");

		System.out.println("\n--------------\n");
	}
}
