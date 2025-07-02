from tower_parser import ClientProgram

ClientProgram.all_to_csv('collapsed.csv', collapse_quota_shares=True)
ClientProgram.all_to_csv('expanded.csv', collapse_quota_shares=False)
ClientProgram.all_to_std_csv('collapsed_std.csv', collapse_quota_shares=True)
ClientProgram.all_to_std_csv('expanded_std.csv', collapse_quota_shares=False)
