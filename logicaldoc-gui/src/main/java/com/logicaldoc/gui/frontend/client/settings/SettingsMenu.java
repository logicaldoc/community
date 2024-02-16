package com.logicaldoc.gui.frontend.client.settings;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.gui.frontend.client.settings.automation.AutomationSettingsPanel;
import com.logicaldoc.gui.frontend.client.settings.comparators.ComparatorsPanel;
import com.logicaldoc.gui.frontend.client.settings.gui.GUISettingsPanel;
import com.logicaldoc.gui.frontend.client.settings.messages.OutgoingEmailPanel;
import com.logicaldoc.gui.frontend.client.settings.protocols.ProtocolsPanel;
import com.logicaldoc.gui.frontend.client.settings.searchindex.SearchIndexPanel;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the administration system configurations menu
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SettingsMenu extends VLayout {

	public SettingsMenu() {
		setMargin(10);
		setMembersMargin(5);
		setOverflow(Overflow.AUTO);

		addSearchAndIndexing();

		addRepositories();

		addGuiSettings();

		addOcr();

		addProtocols();

		addOutgoingEmail();

		addQuota();

		addKeystore();

		addVia();

		addAutomation();

		addComparators();

		addParameters();
	}

	private void addParameters() {
		Button parameters = new Button(I18N.message("parameters"));
		parameters.setWidth100();
		parameters.setHeight(25);
		parameters.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new ParametersPanel()));
		if (Session.get().isDefaultTenant() && Menu.enabled(Menu.PARAMETERS))
			addMember(parameters);
	}

	private void addComparators() {
		Button comparators = new Button(I18N.message("comparators"));
		comparators.setWidth100();
		comparators.setHeight(25);
		comparators.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new ComparatorsPanel()));
		if (Feature.visible(Feature.COMPARISON) && Menu.enabled(Menu.COMPARATORS)) {
			addMember(comparators);
			if (!Feature.enabled(Feature.COMPARISON))
				setFeatureDisabled(comparators);
		}
	}

	private void setFeatureDisabled(Button button) {
		button.setDisabled(true);
		button.setTooltip(I18N.message("featuredisabled"));
	}

	private void addAutomation() {
		Button automation = new Button(I18N.message("automation"));
		automation.setWidth100();
		automation.setHeight(25);
		automation.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new AutomationSettingsPanel()));
		if (Feature.visible(Feature.AUTOMATION) && Menu.enabled(Menu.AUTOMATION)) {
			addMember(automation);
			if (!Feature.enabled(Feature.AUTOMATION))
				setFeatureDisabled(automation);
		}
	}

	private void addVia() {
		Button via = new Button(I18N.message("via"));
		via.setWidth100();
		via.setHeight(25);
		via.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new VIASettingsPanel()));
		if (Feature.visible(Feature.VIA) && Menu.enabled(Menu.VIA)) {
			addMember(via);
			if (!Feature.enabled(Feature.VIA))
				setFeatureDisabled(via);
		}
	}

	private void addKeystore() {
		Button keystore = new Button(I18N.message("keystore"));
		keystore.setWidth100();
		keystore.setHeight(25);
		keystore.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new KeystorePanel()));
		if (Feature.enabled(Feature.DIGITAL_SIGNATURE) && Menu.enabled(Menu.KEYSTORE))
			addMember(keystore);
	}

	private void addQuota() {
		Button quota = new Button(I18N.message("quota"));
		quota.setWidth100();
		quota.setHeight(25);
		quota.addClickHandler(
				(ClickEvent event) -> AdminScreen.get().setContent(new QuotaPanel(Session.get().getTenantId())));
		if (Session.get().isDefaultTenant() && Menu.enabled(Menu.QUOTA))
			addMember(quota);
	}

	private void addOutgoingEmail() {
		Button smtp = new Button(I18N.message("outgoingemail"));
		smtp.setWidth100();
		smtp.setHeight(25);
		smtp.addClickHandler(
				event -> SettingService.Instance.get().loadEmailSettings(new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIEmailSettings settings) {
						AdminScreen.get().setContent(new OutgoingEmailPanel(settings));
					}

				}));

		if (Session.get().isDemo())
			setFeatureDisabled(smtp);

		if (Menu.enabled(Menu.OUTGOING_EMAIL))
			addMember(smtp);
	}

	private void addProtocols() {
		Button protocols = new Button(I18N.message("protocols"));
		protocols.setWidth100();
		protocols.setHeight(25);
		protocols.addClickHandler(event -> AdminScreen.get().setContent(new ProtocolsPanel()));
		if (Feature.visible(Feature.PROTOCOLS) && Menu.enabled(Menu.CLIENTS)) {
			addMember(protocols);
			if (!Feature.enabled(Feature.PROTOCOLS))
				setFeatureDisabled(protocols);
		}
	}

	private void addOcr() {
		Button ocr = new Button(I18N.message("ocr"));
		ocr.setWidth100();
		ocr.setHeight(25);
		ocr.addClickHandler(event -> AdminScreen.get().setContent(new OCRSettingsPanel()));
		if (Feature.visible(Feature.OCR) && Menu.enabled(Menu.OCR)) {
			addMember(ocr);
			if (!Feature.enabled(Feature.OCR))
				setFeatureDisabled(ocr);
		}
	}

	private void addGuiSettings() {
		Button guiSettings = new Button(I18N.message("guisettings"));
		guiSettings.setWidth100();
		guiSettings.setHeight(25);
		guiSettings.addClickHandler(event -> AdminScreen.get().setContent(new GUISettingsPanel()));
		if (Menu.enabled(Menu.GUI_SETTINGS))
			addMember(guiSettings);
	}

	private void addRepositories() {
		Button repositories = new Button(I18N.message("repositories"));
		repositories.setWidth100();
		repositories.setHeight(25);
		repositories.addClickHandler(event -> AdminScreen.get().setContent(new RepositoriesPanel()));
		if (Session.get().isDefaultTenant() && Menu.enabled(Menu.REPOSITORIES))
			addMember(repositories);
	}

	private void addSearchAndIndexing() {
		Button searchAndIndexing = new Button(I18N.message("searchandindexing"));
		searchAndIndexing.setWidth100();
		searchAndIndexing.setHeight(25);
		searchAndIndexing.addClickHandler(event -> AdminScreen.get().setContent(new SearchIndexPanel()));
		if (Menu.enabled(Menu.SEARCH_AND_INDEXING))
			addMember(searchAndIndexing);
	}
}