use std::io::Error;
use std::sync::Arc;

use super::{LangError, Wrappable};
use crate::source::SourceFile;

#[derive(Clone, Debug)]
pub enum LoaderError {
  IO {
    error: Arc<Error>,
    message: String,
  },
  MissingDependencies {
    dependencies: Vec<String>,
    src: SourceFile,
  },
  CyclicDependency {
    cycle: Vec<String>,
  },
  MissingModule {
    module: String,
  },
  NotLoadedModule {
    module: String,
  },
}

impl Wrappable for LoaderError {
  type Wrapper = LangError;

  fn wrap(self) -> Self::Wrapper {
    LangError::Loader(self)
  }
}

/// Custom [PartialEq] implementation because of [Arc] used in IO variant.
impl PartialEq for LoaderError {
  fn eq(&self, other: &LoaderError) -> bool {
    match self {
      // std::io:Error cannot be compared, so we ignore this case.
      | LoaderError::IO { .. } => false,
      | LoaderError::MissingDependencies {
        dependencies: this,
        src: name0,
        ..
      } => {
        if let LoaderError::MissingDependencies {
          dependencies: other,
          src: name1,
          ..
        } = other
        {
          this == other && name0 == name1
        } else {
          false
        }
      },
      | LoaderError::CyclicDependency { cycle: this, .. } => {
        if let LoaderError::CyclicDependency { cycle: other } = other {
          this == other
        } else {
          false
        }
      },
      | LoaderError::MissingModule { module: this, .. } => {
        if let LoaderError::MissingModule { module: other } = other {
          this == other
        } else {
          false
        }
      },
      | LoaderError::NotLoadedModule { module: this, .. } => {
        if let LoaderError::NotLoadedModule { module: other } = other {
          this == other
        } else {
          false
        }
      },
    }
  }
}
