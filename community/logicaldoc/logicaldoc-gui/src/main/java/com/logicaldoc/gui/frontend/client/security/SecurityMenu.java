package com.logicaldoc.gui.frontend.client.security;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUISecuritySettings;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.security.ldap.LDAPServersPanel;
import com.logicaldoc.gui.frontend.client.security.twofactorsauth.TwoFactorsAuthenticationSettings;
import com.logicaldoc.gui.frontend.client.security.user.UsersPanel;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the administration security menu
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SecurityMenu extends VLayout {

	public SecurityMenu() {
		setMargin(10);
		setMembersMargin(5);
		setOverflow(Overflow.AUTO);

		Button users = new Button(I18N.message("users"));
		users.setWidth100();
		users.setHeight(25);

		Button groups = new Button(I18N.message("groups"));
		groups.setWidth100();
		groups.setHeight(25);

		Button security = new Button(I18N.message("security"));
		security.setWidth100();
		security.setHeight(25);

		Button antivirus = new Button(I18N.message("antivirus"));
		antivirus.setWidth100();
		antivirus.setHeight(25);

		Button extAuth = new Button(I18N.message("extauth"));
		extAuth.setWidth100();
		extAuth.setHeight(25);

		Button twoFactorsAuthentication = new Button(I18N.message("twofactorsauth"));
		twoFactorsAuthentication.setWidth100();
		twoFactorsAuthentication.setHeight(25);

		Button firewall = new Button(I18N.message("firewall"));
		firewall.setWidth100();
		firewall.setHeight(25);

		Button bruteForcePrevention = new Button(I18N.message("buteforceattack"));
		bruteForcePrevention.setWidth100();
		bruteForcePrevention.setHeight(25);

		Button singleSingon = new Button(I18N.message("singlesignon"));
		singleSingon.setWidth100();
		singleSingon.setHeight(25);

		List<Button> buttons = new ArrayList<Button>();
		buttons.add(users);
		buttons.add(groups);
		buttons.add(security);

		if (Feature.visible(Feature.ANTIVIRUS)) {
			buttons.add(antivirus);
			if (!Feature.enabled(Feature.ANTIVIRUS) || Session.get().isDemo()) {
				antivirus.setDisabled(true);
				antivirus.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.enabled(Feature.FIREWALL) && Session.get().isDefaultTenant() && Menu.enabled(Menu.FIREWALL)) {
			buttons.add(firewall);
			if (!Feature.enabled(Feature.FIREWALL) || Session.get().isDemo()) {
				firewall.setDisabled(true);
				firewall.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.BRUTEFORCE_ATTACK_PREVENTION) && Session.get().isDefaultTenant()
				&& Menu.enabled(Menu.BRUTEFORCE_ATTACK_PREVENTION)) {
			buttons.add(bruteForcePrevention);
			if (!Feature.enabled(Feature.BRUTEFORCE_ATTACK_PREVENTION) || Session.get().isDemo()) {
				bruteForcePrevention.setDisabled(true);
				bruteForcePrevention.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.TWO_FACTORS_AUTHENTICATION) && Menu.enabled(Menu.TWO_FACTORS_AUTHENTICATION)) {
			buttons.add(twoFactorsAuthentication);
			if (!Feature.enabled(Feature.TWO_FACTORS_AUTHENTICATION) || Session.get().isDemo()) {
				twoFactorsAuthentication.setDisabled(true);
				twoFactorsAuthentication.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.LDAP) && Menu.enabled(Menu.EXTERNAL_AUTHENTICATION)) {
			buttons.add(extAuth);
			if (!Feature.enabled(Feature.LDAP) || Session.get().isDemo()) {
				extAuth.setDisabled(true);
				extAuth.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Feature.visible(Feature.SINGLE_SIGNON) && Session.get().isDefaultTenant()
				&& Menu.enabled(Menu.SINGLE_SIGNON)) {
			buttons.add(singleSingon);
			if (!Feature.enabled(Feature.SINGLE_SIGNON) || Session.get().isDemo()) {
				singleSingon.setDisabled(true);
				singleSingon.setTooltip(I18N.message("featuredisabled"));
			}
		}

		setMembers(buttons.toArray(new Button[0]));

		users.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new UsersPanel());
			}
		});

		groups.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new GroupsPanel());
			}
		});

		firewall.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new FirewallPanel());
			}
		});

		bruteForcePrevention.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new BruteForcePanel());
			}
		});

		security.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SecurityService.Instance.get().loadSettings(new AsyncCallback<GUISecuritySettings>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUISecuritySettings settings) {
						AdminScreen.get().setContent(new SecuritySettingsPanel(settings));
					}

				});
			}
		});

		extAuth.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new LDAPServersPanel());
			}
		});

		twoFactorsAuthentication.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new TwoFactorsAuthenticationSettings());
			}
		});

		singleSingon.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new SingleSignonPanel());
			}
		});

		antivirus.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new AntivirusPanel());
			}
		});
	}
}