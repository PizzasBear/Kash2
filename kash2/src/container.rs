use std::{borrow::Borrow, convert::Infallible, fmt, marker::PhantomData, ops};

#[derive(Debug, Clone, Copy, Default)]
pub struct Owned;
#[derive(Debug, Clone, Copy, Default)]
pub struct Ref<'a>(PhantomData<&'a ()>);

pub trait ContainerMask<'a>: fmt::Debug + Clone + Copy {
    type Owned: fmt::Debug + Clone + Copy;
    type Ref: fmt::Debug + Clone + Copy;
}

impl<'a> ContainerMask<'a> for Owned {
    type Owned = ();
    type Ref = Infallible;
}

impl<'a> ContainerMask<'a> for Ref<'a> {
    type Owned = Infallible;
    type Ref = PhantomData<&'a ()>;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Container<'a, M: ContainerMask<'a>, T: ?Sized + ToOwned> {
    Owned(T::Owned, M::Owned),
    Ref(&'a T, M::Ref),
}

impl<'a, M: ContainerMask<'a>, T: ?Sized + ToOwned> Clone for Container<'a, M, T> {
    fn clone(&self) -> Self {
        match *self {
            Self::Ref(x, y) => Self::Ref(x, y),
            Self::Owned(ref x, y) => Self::Owned(x.borrow().to_owned(), y),
        }
    }
}

impl<'a, M, T> Copy for Container<'a, M, T>
where
    M: ContainerMask<'a>,
    T: ?Sized + ToOwned,
    T::Owned: Copy,
{
}

impl<'a, M: ContainerMask<'a>, T: ?Sized + ToOwned> ops::Deref for Container<'a, M, T> {
    type Target = T;
    fn deref(&self) -> &T {
        match self {
            Self::Owned(x, _) => x.borrow(),
            Self::Ref(x, _) => x,
        }
    }
}

impl<'a, T: ?Sized + ToOwned> From<&'a T> for Container<'static, Owned, T> {
    fn from(x: &'a T) -> Self {
        Self::Owned(x.to_owned(), ())
    }
}

impl<'a, M: ContainerMask<'a>, T: ?Sized + ToOwned> Container<'a, M, T> {
    pub fn into_owned(&self) -> Container<'static, Owned, T> {
        match *self {
            Self::Owned(ref x, _) => Container::Owned(x.borrow().to_owned(), ()),
            Self::Ref(x, _) => Container::Owned(x.to_owned(), ()),
        }
    }
}

pub fn make_owned<T: ?Sized + ToOwned>(x: T::Owned) -> Container<'static, Owned, T> {
    Container::Owned(x, ())
}

fn larp() {
    let container = Container::<Owned, u32>::Owned(1, ());
    let x = &*container;
}

fn loorp<'a, M: ContainerMask<'a>, T: ToOwned>(container: Container<'a, M, T>) {
    let x = &*container;
}

// pub trait ContainerType<T> {
//     type Container;
// }
//
// impl<T: ToOwned> ContainerType<T> for Owned {
//     type Container = T::Owned;
// }
//
// impl<'a, T: 'a> ContainerType<T> for Ref<'a> {
//     type Container = &'a T;
// }
//
// impl<'a, T: 'a> ContainerType<T> for RefMut<'a> {
//     type Container = &'a mut T;
// }
//
// #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
// pub struct Container<C: ContainerType<T>, T>(C::Container);
//
// pub type OwnedContainer<T> = Container<Owned, T>;
// pub type RefContainer<'a, T> = Container<Ref<'a>, T>;
// pub type RefMutContainer<'a, T> = Container<RefMut<'a>, T>;
