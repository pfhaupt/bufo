
#[derive(Default, Debug, Clone, PartialEq)]
pub enum OptimizationLevel {
    #[default]
    None,
    Some,
    All,
    Aggressive,
    Size,
}

impl OptimizationLevel {
    pub fn as_clang_str(&self) -> &str {
        match self {
            Self::None => "",
            Self::Some => "-O1",
            Self::All => "-O2",
            Self::Aggressive => "-O3",
            Self::Size => "-Os"
        }
    }
}

impl From<&OptimizationLevel> for inkwell::OptimizationLevel {
    fn from(level: &OptimizationLevel) -> Self {
        match level {
            OptimizationLevel::None => Self::None,
            OptimizationLevel::Some => Self::Less,
            OptimizationLevel::All => Self::Default,
            OptimizationLevel::Aggressive => Self::Aggressive,
            OptimizationLevel::Size => Self::Default,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct OptimizationFlags {
    pub level: OptimizationLevel,
    pub dead_code: bool,
}

impl From<&str> for OptimizationFlags {
    fn from(s: &str) -> Self {
        match s {
            "0" => Self::none(),
            "1" => Self::some(),
            "2" => Self::all(),
            "3" => Self::aggressive(),
            "s" => Self::size(),
            _ => panic!()
        }
    }
}

impl OptimizationFlags {
    pub fn none() -> Self {
        Self {
            level: OptimizationLevel::None,
            ..Default::default()
        }
    }

    pub fn some() -> Self {
        Self {
            level: OptimizationLevel::Some,
            // Add more optimizations here
            dead_code: true,

            // Also enable all previous optimizations
            ..Self::none()
        }
    }

    pub fn all() -> Self {
        Self {
            level: OptimizationLevel::All,
            // Add more optimizations here

            // Also enable all previous optimizations
            ..Self::some()
        }
    }

    pub fn aggressive() -> Self {
        Self {
            level: OptimizationLevel::Aggressive,
            // Add more optimizations here

            // Also enable all previous optimizations
            ..Self::all()
        }
    }

    pub fn size() -> Self {
        Self {
            level: OptimizationLevel::Size,
            // Add more optimizations here

            // No optimizations yet :^)
            ..Default::default()
        }
    }
}