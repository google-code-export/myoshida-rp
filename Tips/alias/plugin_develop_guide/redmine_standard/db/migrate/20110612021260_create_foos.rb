class CreateFoos < ActiveRecord::Migration
  def self.up
    create_table :foos do |t|
      t.column :project_id, :integer
      t.column :subject, :string
      t.column :description, :text
    end
  end

  def self.down
    drop_table :foos
  end
end
