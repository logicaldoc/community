package com.logicaldoc.gui.frontend.client.reports;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.reports.custom.CustomReportsPanel;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the reports menu
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.1
 */
public class ReportsMenu extends VLayout {
	public ReportsMenu() {
		setMargin(10);
		setMembersMargin(5);
		setOverflow(Overflow.AUTO);

		addLastChangesButton();

		addLockedDocsButton();

		addDeletedDocsButton();

		addDeletedFoldersButton();

		addArchivedDocsButton();

		addDubplicatesButton();

		addCalendarButton();

		addTicketsButton();

		addSubscriptionsButton();

		addApiCallsButton();

		addCustomReportsButton();
	}

	private void addCustomReportsButton() {
		Button customReports = new Button(I18N.message("customreports"));
		customReports.setWidth100();
		customReports.setHeight(25);
		customReports.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new CustomReportsPanel()));
		if (Feature.visible(Feature.CUSTOM_REPORTS) && Menu.enabled(Menu.CUSTOMREPORTS)) {
			addMember(customReports);
			if (!Feature.enabled(Feature.CUSTOM_REPORTS))
				setFeatureDisabled(customReports);
		}
	}

	private void setFeatureDisabled(Button button) {
		button.setDisabled(true);
		button.setTooltip(I18N.message("featuredisabled"));
	}

	private void addApiCallsButton() {
		Button apiCalls = new Button(I18N.message("apicalls"));
		apiCalls.setWidth100();
		apiCalls.setHeight(25);
		apiCalls.addClickHandler(click -> AdminScreen.get().setContent(new ApiCallsReport()));
		if (Menu.enabled(Menu.TICKETS))
			addMember(apiCalls);
	}

	private void addSubscriptionsButton() {
		Button subscriptions = new Button(I18N.message("subscriptions"));
		subscriptions.setWidth100();
		subscriptions.setHeight(25);
		subscriptions.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new SubscriptionsReport()));
		if (Feature.visible(Feature.AUDIT) && Menu.enabled(Menu.SUBSCRIPTIONS_REPORT)) {
			addMember(subscriptions);
			if (!Feature.enabled(Feature.AUDIT))
				setFeatureDisabled(subscriptions);
		}
	}

	private void addTicketsButton() {
		Button tickets = new Button(I18N.message("tickets"));
		tickets.setWidth100();
		tickets.setHeight(25);
		tickets
				.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new TicketsReport()));
		if (Menu.enabled(Menu.TICKETS))
			addMember(tickets);
	}

	private void addCalendarButton() {
		Button calendar = new Button(I18N.message("calendar"));
		calendar.setWidth100();
		calendar.setHeight(25);
		calendar.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new CalendarReport()));
		if (Feature.visible(Feature.CALENDAR) && Menu.enabled(Menu.CALENDAR_REPORT)) {
			addMember(calendar);
			if (!Feature.enabled(Feature.CALENDAR))
				setFeatureDisabled(calendar);
		}
	}

	private void addDubplicatesButton() {
		Button duplicates = new Button(I18N.message("duplicates"));
		duplicates.setWidth100();
		duplicates.setHeight(25);
		duplicates.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new DuplicatesReport()));
		if (Feature.visible(Feature.DUPLICATES_DISCOVERY)) {
			addMember(duplicates);
			if (!Feature.enabled(Feature.DUPLICATES_DISCOVERY)) {
				setFeatureDisabled(duplicates);
			}
		}
	}

	private void addDeletedFoldersButton() {
		Button deletedFolders = new Button(I18N.message("deletedfolders"));
		deletedFolders.setWidth100();
		deletedFolders.setHeight(25);
		deletedFolders.addClickHandler(event -> AdminScreen.get().setContent(new DeletedFoldersReport()));
		if (Menu.enabled(Menu.DELETED_FOLDERS))
			addMember(deletedFolders);
	}

	private void addArchivedDocsButton() {
		Button archivedDocs = new Button(I18N.message("archiveddocs"));
		archivedDocs.setWidth100();
		archivedDocs.setHeight(25);
		archivedDocs.addClickHandler(event -> AdminScreen.get().setContent(new ArchivedDocsReport()));
		if (Feature.visible(Feature.ARCHIVING) && Menu.enabled(Menu.ARCHIVED_DOCS)) {
			addMember(archivedDocs);
			if (!Feature.enabled(Feature.ARCHIVING)) {
				setFeatureDisabled(archivedDocs);
			}
		}
	}

	private void addDeletedDocsButton() {
		Button deletedDocs = new Button(I18N.message("deleteddocs"));
		deletedDocs.setWidth100();
		deletedDocs.setHeight(25);
		deletedDocs.addClickHandler(event -> AdminScreen.get().setContent(new DeletedDocsReport()));
		if (Menu.enabled(Menu.DELETED_DOCS))
			addMember(deletedDocs);
	}

	private void addLockedDocsButton() {
		Button lockedDocs = new Button(I18N.message("lockeddocs"));
		lockedDocs.setWidth100();
		lockedDocs.setHeight(25);
		lockedDocs.addClickHandler(event -> AdminScreen.get().setContent(new LockedDocsReport()));
		if (Menu.enabled(Menu.LOCKED_DOCS))
			addMember(lockedDocs);
	}

	private void addLastChangesButton() {
		Button lastChanges = new Button(I18N.message("lastchanges"));
		lastChanges.setWidth100();
		lastChanges.setHeight(25);
		lastChanges.addClickHandler(event -> AdminScreen.get().setContent(new LastChangesReport()));
		if (Menu.enabled(Menu.LAST_CHANGES))
			addMember(lastChanges);
	}
}
