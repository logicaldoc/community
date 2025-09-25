package com.logicaldoc.gui.frontend.client.impex;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.impex.archives.ExportArchivesPanel;
import com.logicaldoc.gui.frontend.client.impex.archives.ImportArchivesPanel;
import com.logicaldoc.gui.frontend.client.impex.converters.FormatConvertersPanel;
import com.logicaldoc.gui.frontend.client.impex.email.EmailAccountsPanel;
import com.logicaldoc.gui.frontend.client.impex.folders.ImportFoldersPanel;
import com.logicaldoc.gui.frontend.client.impex.syndication.SyndicationsPanel;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the administration of import/export features.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class ImpexMenu extends VLayout {

	public ImpexMenu() {
		setMargin(10);
		setMembersMargin(5);
		setOverflow(Overflow.AUTO);

		addImportFoldersButton();

		addEmailsButton();

		addImportArchivesButton();

		addExportArchivesButton();

		addConvertersButton();

		addSyndicationButton();
	}

	private void addSyndicationButton() {
		Button syndication = new Button(I18N.message("syndication"));
		syndication.setWidth100();
		syndication.setHeight(25);
		syndication.addClickHandler(click -> AdminScreen.get().setContent(new SyndicationsPanel()));
		if (Feature.visible(Feature.SYNDICATION) && Menu.enabled(Menu.SYNDICATION)) {
			addMember(syndication);
			if (!Feature.enabled(Feature.SYNDICATION))
				setFeatureDisabled(syndication);
		}
	}

	private void addConvertersButton() {
		Button converters = new Button(I18N.message("formatconverters"));
		converters.setWidth100();
		converters.setHeight(25);
		converters.addClickHandler(click -> AdminScreen.get().setContent(new FormatConvertersPanel()));

		if (Session.get().isDefaultTenant() && Feature.visible(Feature.FORMAT_CONVERSION)
				&& Menu.enabled(Menu.FORMAT_CONVERTERS)) {
			addMember(converters);
			if (!Feature.enabled(Feature.FORMAT_CONVERSION))
				setFeatureDisabled(converters);
		}
	}

	private void addExportArchivesButton() {
		Button exportArchives = new Button(I18N.message("exportarchives"));
		exportArchives.setWidth100();
		exportArchives.setHeight(25);
		exportArchives.addClickHandler(
				(ClickEvent exportArchivesClick) -> AdminScreen.get().setContent(new ExportArchivesPanel()));
		if (Feature.visible(Feature.IMPEX)) {
			addMember(exportArchives);
			if (!Feature.enabled(Feature.IMPEX))
				setFeatureDisabled(exportArchives);
		}
	}

	private void addImportArchivesButton() {
		Button importArchives = new Button(I18N.message("importarchives"));
		importArchives.setWidth100();
		importArchives.setHeight(25);
		importArchives.addClickHandler(
				(ClickEvent importArchivesClick) -> AdminScreen.get().setContent(new ImportArchivesPanel()));
		if (Feature.visible(Feature.IMPEX)) {
			addMember(importArchives);
			if (!Feature.enabled(Feature.IMPEX))
				setFeatureDisabled(importArchives);
		}
	}

	private void addEmailsButton() {
		Button emails = new Button(I18N.message("emailaccounts"));
		emails.setWidth100();
		emails.setHeight(25);
		emails.addClickHandler((ClickEvent emailClick) -> AdminScreen.get().setContent(new EmailAccountsPanel()));
		if (Feature.visible(Feature.EMAIL_IMPORT)) {
			addMember(emails);
			if (!Feature.enabled(Feature.EMAIL_IMPORT))
				setFeatureDisabled(emails);
		}
	}

	private void addImportFoldersButton() {
		Button importFolders = new Button(I18N.message("importfolders"));
		importFolders.setWidth100();
		importFolders.setHeight(25);
		importFolders
				.addClickHandler((ClickEvent importClick) -> AdminScreen.get().setContent(new ImportFoldersPanel()));
		if (Feature.visible(Feature.IMPORT_REMOTE_FOLDERS) || Feature.visible(Feature.IMPORT_LOCAL_FOLDERS)) {
			addMember(importFolders);
			if (!Feature.enabled(Feature.IMPORT_REMOTE_FOLDERS) && !Feature.enabled(Feature.IMPORT_LOCAL_FOLDERS))
				setFeatureDisabled(importFolders);
		}
	}

	private void setFeatureDisabled(Button button) {
		button.setDisabled(true);
		button.setTooltip(I18N.message("featuredisabled"));
	}
}
