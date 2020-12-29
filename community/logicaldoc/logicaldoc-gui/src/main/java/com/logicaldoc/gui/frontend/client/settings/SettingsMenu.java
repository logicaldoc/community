package com.logicaldoc.gui.frontend.client.settings;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.gui.frontend.client.settings.automation.AutomationSettingsPanel;
import com.logicaldoc.gui.frontend.client.settings.comparators.ComparatorsPanel;
import com.logicaldoc.gui.frontend.client.settings.gui.GUISettingsPanel;
import com.logicaldoc.gui.frontend.client.settings.messages.OutgoingEmailPanel;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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

		Button searchAndIndexing = new Button(I18N.message("searchandindexing"));
		searchAndIndexing.setWidth100();
		searchAndIndexing.setHeight(25);
		addMember(searchAndIndexing);

		Button repositories = new Button(I18N.message("repositories"));
		repositories.setWidth100();
		repositories.setHeight(25);
		if (Session.get().isDefaultTenant() && Menu.enabled(Menu.REPOSITORIES))
			addMember(repositories);

		Button guiSettings = new Button(I18N.message("guisettings"));
		guiSettings.setWidth100();
		guiSettings.setHeight(25);
		addMember(guiSettings);

		Button quota = new Button(I18N.message("quota"));
		quota.setWidth100();
		quota.setHeight(25);

		Button keystore = new Button(I18N.message("keystore"));
		keystore.setWidth100();
		keystore.setHeight(25);

		Button ocr = new Button(I18N.message("ocr"));
		ocr.setWidth100();
		ocr.setHeight(25);
		if (Feature.visible(Feature.OCR)) {
			addMember(ocr);
			if (!Feature.enabled(Feature.OCR)) {
				ocr.setDisabled(true);
				ocr.setTooltip(I18N.message("featuredisabled"));
			}
		}

		Button smtp = new Button(I18N.message("outgoingemail"));
		smtp.setWidth100();
		smtp.setHeight(25);
		if (Session.get().isDemo()) {
			smtp.setDisabled(true);
			smtp.setTooltip(I18N.message("featuredisabled"));
		}

		Button protocols = new Button(I18N.message("protocols"));
		protocols.setWidth100();
		protocols.setHeight(25);

		Button via = new Button(I18N.message("via"));
		via.setWidth100();
		via.setHeight(25);

		Button automation = new Button(I18N.message("automation"));
		automation.setWidth100();
		automation.setHeight(25);

		Button comparators = new Button(I18N.message("comparators"));
		comparators.setWidth100();
		comparators.setHeight(25);

		Button parameters = new Button(I18N.message("parameters"));
		parameters.setWidth100();
		parameters.setHeight(25);

		if (Feature.visible(Feature.PROTOCOLS) && Menu.enabled(Menu.CLIENTS)) {
			addMember(protocols);
			if (!Feature.enabled(Feature.PROTOCOLS)) {
				protocols.setDisabled(true);
				protocols.setTooltip(I18N.message("featuredisabled"));
			}
		}

		protocols.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new ProtocolsPanel());
			}
		});

		quota.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new QuotaPanel(Session.get().getTenantId()));
			}
		});

		keystore.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new KeystorePanel());
			}
		});

		via.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new VIASettingsPanel());
			}
		});

		comparators.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new ComparatorsPanel());
			}
		});

		automation.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new AutomationSettingsPanel());
			}
		});

		addMember(smtp);

		if (Session.get().isDefaultTenant() && Menu.enabled(Menu.QUOTA))
			addMember(quota);

		if (Feature.enabled(Feature.DIGITAL_SIGNATURE) && Menu.enabled(Menu.KEYSTORE))
			addMember(keystore);

		if (Feature.visible(Feature.VIA) && Menu.enabled(Menu.VIA)) {
			addMember(via);
			if (!Feature.enabled(Feature.VIA)) {
				via.setDisabled(true);
				via.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.AUTOMATION) && Menu.enabled(Menu.AUTOMATION)) {
			addMember(automation);
			if (!Feature.enabled(Feature.AUTOMATION)) {
				automation.setDisabled(true);
				automation.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.COMPARISON) && Menu.enabled(Menu.COMPARATORS)) {
			addMember(comparators);
			if (!Feature.enabled(Feature.COMPARISON)) {
				comparators.setDisabled(true);
				comparators.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Session.get().isDefaultTenant() && Menu.enabled(Menu.PARAMETERS))
			addMember(parameters);

		searchAndIndexing.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new SearchIndexingPanel());
			}
		});

		guiSettings.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new GUISettingsPanel());
			}
		});

		ocr.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new OCRSettingsPanel());
			}
		});

		parameters.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SettingService.Instance.get().loadSettings(new AsyncCallback<GUIParameter[]>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIParameter[] settings) {
						AdminScreen.get().setContent(new ParametersPanel(settings));
					}

				});
			}
		});

		smtp.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SettingService.Instance.get().loadEmailSettings(new AsyncCallback<GUIEmailSettings>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIEmailSettings settings) {
						AdminScreen.get().setContent(new OutgoingEmailPanel(settings));
					}

				});
			}
		});

		repositories.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new RepositoriesPanel());
			}
		});
	}
}