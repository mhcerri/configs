-- TODO: copy it as config.lua and edit the configs bellow:
-- Email account
pass = io.popen('gpg2 -dq ~/.mutt/my_pass.gpg', 'r'):read('*a')
acct = IMAP {
	server = 'imap.gmail.com',
	username = '<email>',
	password = pass,
	ssl = 'ssl23',
}

-- Move important bugs
lp_user = 'mhcerri'
bugs = function()
	return acct['Bugs']:is_recent()
end
results = bugs():contain_field('X-Launchpad-Message-For', lp_user)
		:contain_field('X-Launchpad-Message-Rationale', 'Subscriber')
		:match_field('X-Launchpad-Message-Rationale', '^Subscriber$') +
	bugs():contain_field('X-Launchpad-Bug-Commenters', lp_user) +
	bugs():contain_field('X-Launchpad-Bug-Reporter', lp_user) +
	bugs():contain_field('X-Launchpad-Bug-Modifier', lp_user)
results:move_messages(acct['Bugs/Important'])
