module InfoHelper

  def is_shown?(catsym)
    return (User.current.admin? or InfoCategory::is_shown?(catsym));
  end

  def is_admin_only?(catsym)
    return (User.current.admin? and !InfoCategory::is_shown?(catsym));
  end

  def workflows_empty?(statuses, workflows)
    for old_status in @statuses
      for new_status in @statuses
        hit = workflows.detect {|w|
          w.old_status_id == old_status.id && w.new_status_id == new_status.id
        }
        return false if hit
      end
    end
    return true
  end

  def workflow_has_author_assignee
    (1 < Redmine::VERSION::MAJOR ||
        (1 == Redmine::VERSION::MAJOR && 2 <= Redmine::VERSION::MINOR))
  end
  
end
